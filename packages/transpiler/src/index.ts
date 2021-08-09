import * as abaplint from "@abaplint/core";
import {Validation, config} from "./validation";
import {Traversal} from "./traversal";
import {Requires} from "./requires";
import {SkipSettings, UnitTest} from "./unit_test";
import {Keywords} from "./keywords";
import {DatabaseSetup} from "./database_setup";
import {Rearranger} from "./rearranger";
import {Chunk} from "./chunk";

export {config};

export interface IFile {
  filename: string,
  // from output folder to original source folder
  relative?: string,
  contents: string,
}

export interface IObjectIdentifier {
  name: string,
  type: string,
}

export interface IOutput {
  objects: IOutputFile[];
  reg: abaplint.IRegistry;
  /** Output experimental file to run unit tests */
  unitTest: string;
  databaseSetup: string;
}

export interface IRequire {
  name: string | undefined,
  filename: string,
}

export interface IProgress {
  set(total: number, text: string): void;
  tick(text: string): Promise<void>;
}

/** one javascript output file for each object */
export interface IOutputFile {
  object: IObjectIdentifier;
  filename: string,
  chunk: Chunk,
  requires: readonly IRequire[];
  exports: readonly string[];
}

export interface ITranspilerOptions {
  /** ignore syntax check, used for internal testing */
  ignoreSyntaxCheck?: boolean;
  /** adds common js modules */
  addCommonJS?: boolean;
  /** adds filenames as comments in the output js */
  addFilenames?: boolean;
  /** skip outputing constants, used for internal testing */
  skipConstants?: boolean;
  /** sets behavior for unknown types, either fail at compile- or run-time */
  unknownTypes?: "compileError" | "runtimeError";
  skip?: SkipSettings;
}

export class Transpiler {
  private readonly options: ITranspilerOptions | undefined;

  public constructor(options?: ITranspilerOptions) {
    this.options = options;
    if (this.options === undefined) {
      this.options = {};
    }
    if (this.options.unknownTypes === undefined) {
      this.options.unknownTypes = "compileError";
    }
  }

  public async run(reg: abaplint.IRegistry, progress?: IProgress): Promise<IOutput> {

    reg.parse();
    new Keywords().handle(reg);
    this.validate(reg);

    const dbSetup = new DatabaseSetup(reg).run();

    const output: IOutput = {
      objects: [],
      unitTest: new UnitTest().run(reg, dbSetup, this.options?.skip),
      databaseSetup: dbSetup,
      reg: reg,
    };

    progress?.set(reg.getObjectCount(false), "Building, Syntax Logic");
    for (const obj of reg.getObjects()) {
      await progress?.tick("Building, Syntax Logic, " + obj.getName());
      if (obj instanceof abaplint.ABAPObject) {
        new abaplint.SyntaxLogic(reg, obj).run();
      }
    }

    progress?.set(reg.getObjectCount(false), "Building");
    for (const obj of reg.getObjects()) {
      await progress?.tick("Building, " + obj.getName());
      if (obj instanceof abaplint.ABAPObject && !(obj instanceof abaplint.Objects.TypePool)) {
        output.objects = output.objects.concat(this.runObject(obj, reg));
      }
    }

    return output;
  }

// ///////////////////////////////

  protected handleConstants(obj: abaplint.ABAPObject, file: abaplint.ABAPFile, reg: abaplint.IRegistry): string {
    let result = "";
    const constants = this.findConstants(obj, file, reg);

    if (this.options?.skipConstants === false || this.options?.skipConstants === undefined) {
      for (const c of Array.from(constants).sort()) {
        const post = c < 0 ? "minus_" : "";
        result += `const constant_${post}${Math.abs(c)} = new abap.types.Integer().set(${c});\n`;
      }
    }

    return result;
  }

  protected findConstants(obj: abaplint.ABAPObject, file: abaplint.ABAPFile, reg: abaplint.IRegistry): Set<number> {
    let constants = new Set<number>();

    for (const i of file.getStructure()?.findAllExpressions(abaplint.Expressions.Integer) || []) {
      const j = parseInt(i.concatTokens(), 10);
      constants.add(j);
    }

    // extra constants from interfaces, used for default values
    if (obj.getType() === "CLAS") {
      const clas = obj as abaplint.Objects.Class;
      for (const i of clas.getClassDefinition()?.interfaces || []) {
        const intf = reg.getObject("INTF", i.name) as abaplint.ABAPObject | undefined;
        const main = intf?.getMainABAPFile();
        if (intf && main) {
          constants = new Set([...constants, ...this.findConstants(intf, main, reg).values()]);
        }
      }
    }

    return constants;
  }

  protected runObject(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): IOutputFile[] {
    const spaghetti = new abaplint.SyntaxLogic(reg, obj).run().spaghetti;

    let ret: IOutputFile[] = [];

    for (const file of obj.getSequencedFiles()) {
      const chunk = new Chunk();

      if (this.options?.addFilenames === true) {
        chunk.appendString("// " + file.getFilename() + "\n");
      }

      chunk.appendString(this.handleConstants(obj, file, reg));

      const rearranged = new Rearranger().run(obj.getType(), file.getStructure());

      const contents = new Traversal(spaghetti, file, obj, reg, this.options?.unknownTypes === "runtimeError").traverse(rearranged);
      chunk.appendChunk(contents);
      chunk.stripLastNewline();
      chunk.runIndentationLogic();

      const exports = this.findExports(file.getStructure());
      const filename = file.getFilename().replace(".abap", ".mjs").toLowerCase();

      const output: IOutputFile = {
        object: {
          name: obj.getName(),
          type: obj.getType(),
        },
        filename: filename,
        chunk: chunk,
        requires: new Requires(reg).find(obj, spaghetti.getTop(), file.getFilename()),
        exports: exports,
      };

      ret.push(output);
    }

    ret = this.rearrangeClassLocals(obj, ret);

    if (this.options?.addCommonJS === true) {
      ret.map(output => output.chunk = this.addImportsAndExports(output));
    }

    return ret;
  }

  /** merges the locals def and imp into one mjs file */
  private rearrangeClassLocals(obj: abaplint.ABAPObject, output: IOutputFile[]): IOutputFile[] {
    const ret: IOutputFile[] = [];
    if (obj.getType() !== "CLAS") {
      return output;
    }

    let imp: IOutputFile | undefined = undefined;
    let def: IOutputFile | undefined = undefined;
    for (const o of output) {
      if (o.filename.endsWith(".clas.locals_imp.mjs")) {
        imp = o;
      } else if (o.filename.endsWith(".clas.locals_def.mjs")) {
        def = o;
      } else {
        ret.push(o);
      }
    }

    if (def) {
      def.filename = def.filename.replace(".locals_def.mjs", ".locals.mjs");
    }
    if (imp) {
      imp.filename = imp.filename.replace(".locals_imp.mjs", ".locals.mjs");
    }

    if (imp && def) {
// remove duplicates
      const requires = [...def.requires];
      for (const r of imp.requires) {
        if (requires.find(a => a.filename === r.filename && a.name === r.name) === undefined) {
          requires.push(r);
        }
      }

      const chunk = new Chunk().appendChunk(def.chunk).appendChunk(imp.chunk);

      ret.push({
        object: imp.object,
        filename: imp.filename,
        chunk: chunk,
        requires: requires,
        exports: def.exports.concat(imp.exports),
      });
    } else if (imp) {
      ret.push(imp);
    } else if (def) {
      ret.push(def);
    }

    return ret;
  }

  protected addImportsAndExports(output: IOutputFile): Chunk {
    const contents = new Chunk();
    for (const r of output.requires) {
      const name = r.name?.toLowerCase();
      const filename = r.filename.replace(".abap", ".mjs");
      if (filename === output.filename) {
        continue;
      }
      if (name) {
        contents.appendString("const {" + name + "} = await import(\"./" + filename + "\");\n");
      } else {
        contents.appendString("await import(\"./" + filename + "\");\n");
      }
    }
    contents.appendChunk(output.chunk);
    if (output.exports.length > 0) {
      contents.appendString("\nexport {" + output.exports.join(", ") + "};");
    }
    return contents;
  }

  protected findExports(node: abaplint.Nodes.StructureNode | undefined): string[] {
    if (node === undefined) {
      return [];
    }
    const res: string[] = [];
    for (const c of node.findAllStatements(abaplint.Statements.ClassDefinition)) {
      const e = c.findFirstExpression(abaplint.Expressions.ClassName)?.getFirstToken().getStr();
      if (e) {
        res.push(e.toLowerCase());
      }
    }
    for (const c of node.findAllStatements(abaplint.Statements.Interface)) {
      const e = c.findFirstExpression(abaplint.Expressions.InterfaceName)?.getFirstToken().getStr();
      if (e) {
        res.push(e.toLowerCase());
      }
    }
    return res;
  }

  protected validate(reg: abaplint.IRegistry): void {
    const issues = new Validation(this.options).run(reg);
    if (issues.length > 0) {
      const messages = issues.map(i => i.getKey() + ", " +
        i.getMessage() + ", " +
        i.getFilename() + ":" +
        i.getStart().getRow());
      throw new Error(messages.join("\n"));
    }
  }

}
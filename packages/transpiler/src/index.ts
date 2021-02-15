import * as abaplint from "@abaplint/core";
import {Validation, config} from "./validation";
import {Indentation} from "./indentation";
import {Traversal} from "./traversal";
import {Requires} from "./requires";
import {SkipSettings, UnitTest} from "./unit_test";
import {Keywords} from "./keywords";
import {DatabaseSetup} from "./database_setup";
import  {Rearranger} from "./rearranger";

export {config};

export interface IFile {
  filename: string,
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
  name: string,
  filename: string,
}

export interface IProgress {
  set(total: number, text: string): void;
  tick(text: string): Promise<void>;
}

/** one javascript output file for each object */
export interface IOutputFile {
  object: IObjectIdentifier;
  js: IFile;
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

  public async run(files: IFile[], progress?: IProgress): Promise<IOutput> {
    const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
    const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
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
      if (obj instanceof abaplint.ABAPObject) {
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

    const ret: IOutputFile[] = [];

    for (const file of obj.getSequencedFiles()) {
      let exports: string[] = [];
      let result = "";

      if (this.options?.addFilenames === true) {
        result += "// " + file.getFilename() + "\n";
      }

      result += this.handleConstants(obj, file, reg);

      const rearranged = obj.getType() === "INTF" ? file.getStructure() : new Rearranger().run(file.getStructure());
      const contents = new Traversal(spaghetti, file, obj, reg).traverse(rearranged);
      result += new Indentation().run(contents);

      exports = exports.concat(this.findExports(file.getStructure()));

      if (result.endsWith("\n")) {
        result = result.substring(0, result.length - 1);
      }

      const filename = file.getFilename().replace(".abap", ".js");

      const output: IOutputFile = {
        object: {name: obj.getName(), type: obj.getType()},
        js: {filename: filename.toLowerCase(), contents: result},
        requires: new Requires(reg).find(obj, spaghetti.getTop(), file.getFilename()),
        exports,
      };

      if (this.options?.addCommonJS === true) {
        output.js.contents = this.addCommonJS(output);
      }
      ret.push(output);
    }

    return ret;
  }

  /** adds common js modules syntax */
  protected addCommonJS(output: IOutputFile): string {
    let contents = "";
    for (const r of output.requires) {
      const name = r.name.toLowerCase();
      const filename = r.filename.replace(".abap", ".js");
      contents += "const " + name + " = require(\"./" + filename + "\")." + name + ";\n";
    }
    contents += output.js.contents;
    if (output.exports.length > 0) {
      contents += "\nmodule.exports = {" + output.exports.join(", ") + "};";
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
        res.push(e);
      }
    }
    for (const c of node.findAllStatements(abaplint.Statements.Interface)) {
      const e = c.findFirstExpression(abaplint.Expressions.InterfaceName)?.getFirstToken().getStr();
      if (e) {
        res.push(e);
      }
    }
    return res;
  }

  protected validate(reg: abaplint.IRegistry): void {
    const issues = new Validation(this.options).run(reg);
    if (issues.length > 0) {
      const messages = issues.map(i => i.getKey() + ", " +
        i.getMessage() + ", " +
        i.getFilename() + ", " +
        i.getStart().getRow());
      throw new Error(messages.join("\n"));
    }
  }

}
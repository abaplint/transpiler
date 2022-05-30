import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions} from "./types";
import {Traversal} from "./traversal";
import {Requires} from "./requires";
import {Rearranger} from "./rearranger";
import {Chunk} from "./chunk";

export class HandleABAP {
  private readonly options: ITranspilerOptions | undefined;

  public constructor(options?: ITranspilerOptions) {
    this.options = options;
  }

  public runObject(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): IOutputFile[] {
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
        contents.appendString("const {" + Traversal.escapeClassName(name) + "} = await import(\"./" + filename.replace(/#/g, "%23") + "\");\n");
      } else {
        contents.appendString("await import(\"./" + filename.replace(/#/g, "%23") + "\");\n");
      }
    }
    contents.appendChunk(output.chunk);
    if (output.exports.length > 0) {
      contents.appendString("\nexport {" + output.exports.map(Traversal.escapeClassName).join(", ") + "};");
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


  protected handleConstants(obj: abaplint.ABAPObject, file: abaplint.ABAPFile, reg: abaplint.IRegistry): string {
    let result = "";
    const constants = this.findConstants(obj, file, reg);

    if (this.options?.skipConstants === false || this.options?.skipConstants === undefined) {
      for (const concat of Array.from(constants).sort()) {
        const post = concat.startsWith("-") ? "minus_" : "";
        result += `const constant_${post}${concat.replace("-", "")} = new abap.types.Integer().set(${concat});\n`;
      }
    }

    return result;
  }

  protected findConstants(obj: abaplint.ABAPObject, file: abaplint.ABAPFile, reg: abaplint.IRegistry): Set<string> {
    let constants = new Set<string>();

    for (const i of file.getStructure()?.findAllExpressions(abaplint.Expressions.Integer) || []) {
      constants.add(i.concatTokens().replace(" ", ""));
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

}
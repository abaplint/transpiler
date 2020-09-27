import * as abaplint from "@abaplint/core";
import {Validation, config} from "./validation";
import {Indentation} from "./indentation";
import {Traversal} from "./traversal";
import {Requires} from "./requires";

export {config};

export interface IFile {
  filename: string,
  contents: string,
}

export interface IObjectIdentifier {
  name: string,
  type: string,
}

/** one javascript output file for each object */
export interface IOutput {
  object: IObjectIdentifier;
  js: IFile;
  requires: readonly IObjectIdentifier[];
  exports: readonly string[];
}

export interface ITranspilerOptions {
  ignoreSyntaxCheck?: boolean;
  addCommonJS?: boolean;
}

export class Transpiler {
  private readonly options: ITranspilerOptions | undefined;

  public constructor(options?: ITranspilerOptions) {
    this.options = options;
  }

  public async run(files: IFile[]): Promise<IOutput[]> {
    const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
    const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
    this.validate(reg);

    const output: IOutput[] = [];

    for (const abap of reg.getObjects()) {
      if (abap.getType() === "INTF") {
        continue;
      } else if (abap instanceof abaplint.ABAPObject) {
        output.push(this.runObject(abap, reg));
      }
    }

    return output;
  }

// ///////////////////////////////

  protected runObject(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): IOutput {
    const spaghetti = new abaplint.SyntaxLogic(reg, obj).run().spaghetti;

    let result = "";
    let exports: string[] = [];
    for (const file of obj.getSequencedFiles()) {

      const contents = new Traversal(spaghetti, file, obj).traverse(file.getStructure());
      if (contents.length > 0) {
        result += new Indentation().run(contents);
      }

      exports = exports.concat(this.findExports(file.getStructure()));
    }

    if (result.endsWith("\n")) {
      result = result.substring(0, result.length - 1);
    }

    const filename = obj.getName() + "." + obj.getType() + ".js";
    const output: IOutput = {
      object: {name: obj.getName(), type: obj.getType()},
      js: {filename: filename.toLowerCase(), contents: result},
      requires: new Requires(reg).find(spaghetti.getTop()),
      exports,
    };

    if (this.options?.addCommonJS === true) {
      output.js.contents = this.addCommonJS(output);
    }

    return output;
  }

  /** adds common js modules syntax */
  protected addCommonJS(output: IOutput): string {
    let contents = "";
    for (const r of output.requires) {
      const name = r.name.toLowerCase();
      const filename = name + "." + r.type.toLowerCase() + ".js";
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
    return res;
  }

  protected validate(reg: abaplint.IRegistry): void {
    const issues = new Validation(this.options).run(reg);
    if (issues.length > 0) {
      const messages = issues.map(i => i.getMessage() + ", " + i.getFilename() + ", " + i.getStart().getRow());
      throw new Error(messages.join("\n"));
    }
  }

}
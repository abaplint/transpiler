import {Nodes, MemoryFile, Registry, Structures} from "abaplint";
import {Validation} from "./validation";
import * as StatementTranspilers from "./statements";
import {Indentation} from "./indentation";

export interface IFile {
  filename: string,
  contents: string,
}

export class Transpiler {

  public runMulti(files: IFile[]): IFile[] {
    const memory = files.map(f => new MemoryFile(f.filename, f.contents));
    const reg = new Registry().addFiles(memory);
    this.validate(reg);

    const output: IFile[] = [];
    for (const abap of reg.getABAPObjects()) {
      for (const f of abap.getABAPFiles()) {
        const contents = this.runObject(f.getStructure()!);
        if (contents.length > 0) {
          const filename = f.getFilename().replace(new RegExp("\.abap$"), ".js");
          output.push({filename, contents});
        }
      }
    }
    return output;
  }

// todo, deprecate/remove this method
  public run(code: string): string {
    const reg = new Registry().addFile(new MemoryFile("zfoobar.prog.abap", code));

    this.validate(reg);

    const abap = reg.getABAPObjects()[0].getABAPFiles()[0];

    return this.runObject(abap.getStructure()!);
  }

// ///////////////////////////////

  protected runObject(node: Nodes.StructureNode) {
    let result = this.traverseStructure(node);

    if (result.endsWith("\n")) {
      result = result.substring(0, result.length - 1);
    }

    return new Indentation().run(result);
  }

  protected validate(reg: Registry) {
    const issues = Validation.run(reg);
    if (issues.length > 0) {
      const messages = issues.map(i => i.getMessage());
      throw new Error(messages.join("\n"));
    }
  }

  protected traverseStructure(node: Nodes.StructureNode): string {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c instanceof Nodes.StructureNode) {
        if (c.get() instanceof Structures.Interface
            || c.get() instanceof Structures.ClassDefinition) {
          continue;
        }
        ret = ret + this.traverseStructure(c);
        if (c.get() instanceof Structures.When) {
          ret = ret + "break;\n";
        }
      } else if (c instanceof Nodes.StatementNode) {
        ret = ret + this.traverseStatement(c) + "\n";
      } else {
        throw new Error("traverseStructure, unexpected node type");
      }
    }
    return ret;
  }

  protected traverseStatement(node: Nodes.StatementNode): string {
    const list: any = StatementTranspilers;
    for (const key in list) {
      const transpiler = new list[key]();
      if (node.get().constructor.name + "Transpiler" === transpiler.constructor.name) {
        return transpiler.transpile(node);
      }
    }

    return "todo, statement: " + node.get().constructor.name;
  }

}
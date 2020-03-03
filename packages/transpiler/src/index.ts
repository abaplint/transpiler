import {Nodes, MemoryFile, Registry, Structures} from "abaplint";
import {Validation} from "./validation";
import * as StatementTranspilers from "./statements";
import {Indentation} from "./indentation";

export class Transpiler {

  public run(code: string): string {
    const file = new MemoryFile("zfoobar.prog.abap", code);
    const reg = new Registry().addFile(file);

    const issues = Validation.run(reg);
    if (issues.length > 0) {
      const messages = issues.map(i => i.getMessage());
      throw new Error(messages.join("\n"));
    }

    const abap = reg.getABAPObjects()[0].getABAPFiles()[0];

    let result = this.traverseStructure(abap.getStructure()!);

    if (result.endsWith("\n")) {
      result = result.substring(0, result.length - 1);
    }

    return new Indentation().run(result);
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
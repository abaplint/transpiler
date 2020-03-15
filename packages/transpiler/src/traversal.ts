import {Nodes, Structures, SpaghettiScope, INode} from "abaplint";
import * as StatementTranspilers from "./statements";
// import {ClassImplementationTranspiler} from "./structures/";

export class Traversal {
  private readonly spaghetti: SpaghettiScope;
  private readonly filename: string;

  public constructor(spaghetti: SpaghettiScope, filename: string) {
    this.spaghetti = spaghetti;
    this.filename = filename;
  }

  public traverse(node: INode | undefined): string {
    if (node instanceof Nodes.StructureNode) {
      return this.traverseStructure(node);
    } else if (node instanceof Nodes.StatementNode) {
      return this.traverseStatement(node);
    } else {
      throw new Error("Traverse, unexpected node type");
    }
  }

  protected traverseStructure(node: Nodes.StructureNode): string {
    let ret = "";
// todo, refactor
    for (const c of node.getChildren()) {
      if (c instanceof Nodes.StructureNode) {
        const g = c.get();
        if (g instanceof Structures.Interface
            || g instanceof Structures.Types
            || g instanceof Structures.ClassDefinition) {
          continue;
          /*
        } else if (g instanceof Structures.ClassImplementation) {
          ret = ret + new ClassImplementationTranspiler();
          continue;
          */
        }

        ret = ret + this.traverseStructure(c);
        if (g instanceof Structures.When) {
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
        return transpiler.transpile(node, this.spaghetti, this.filename);
      }
    }
    throw new Error(`Statement ${node.get().constructor.name} not supported`);
  }

}
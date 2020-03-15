import {Nodes, Structures, SpaghettiScope, INode} from "abaplint";
import * as StatementTranspilers from "./statements";
import {ClassImplementationTranspiler} from "./structures/";
import {IStatementTranspiler} from "./statements/_statement_transpiler";

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

  public getFilename(): string {
    return this.filename;
  }

  public getSpaghetti(): SpaghettiScope {
    return this.spaghetti;
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
        } else if (g instanceof Structures.ClassImplementation) {
          ret = ret + new ClassImplementationTranspiler().transpile(c, this);
          continue;
        }

        ret = ret + this.traverseStructure(c);
        if (g instanceof Structures.When) {
          ret = ret + "break;\n";
        }
      } else if (c instanceof Nodes.StatementNode) {
        ret = ret + this.traverseStatement(c);
      } else {
        throw new Error("traverseStructure, unexpected node type");
      }
    }
    return ret;
  }

  protected traverseStatement(node: Nodes.StatementNode): string {
    const list: any = StatementTranspilers;
    for (const key in list) {
      if (node.get().constructor.name + "Transpiler" === key) {
        const transpiler = new list[key]() as IStatementTranspiler;
        return transpiler.transpile(node, this) + "\n";
      }
    }
    throw new Error(`Statement ${node.get().constructor.name} not supported`);
  }

}
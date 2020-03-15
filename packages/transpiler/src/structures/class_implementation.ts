import * as abaplint from "abaplint";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";

export class ClassImplementationTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): string {

    let ret = "";
    for (const c of node.getChildren()) {
      ret = ret + traversal.traverse(c);
      if (c instanceof abaplint.Nodes.StatementNode
          && c.get() instanceof abaplint.Statements.ClassImplementation
          && this.hasConstructor(node) === false) {
        ret = ret + this.buildConstructor(c, traversal);
      }
    }

    return ret;
  }

  private hasConstructor(node: abaplint.Nodes.StructureNode): boolean {
    for (const m of node.findAllStatements(abaplint.Statements.Method)) {
      const name = m.findFirstExpression(abaplint.Expressions.MethodName)?.getFirstToken().getStr();
      if (name?.toUpperCase() === "CONSTRUCTOR") {
        return true;
      }
    }
    return false;
  }

  private buildConstructor(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const scope = traversal.getSpaghetti().lookupPosition(node.getFirstToken().getStart(), traversal.getFilename());

    const ret = traversal.buildConstructorContents(scope);
    if (ret === "") {
      return ret;
    }

    return "constructor() {\n" + ret + "}\n";

    /*
    const vars = scope?.getData().vars;
    if (vars === undefined || vars.length === 0) {
      return "";
    }
    let ret = "constructor() {\n";
    for (const v of vars) {
      ret = ret + "this." + v.name + " = " + new TranspileTypes().toType(v.identifier.getType()) + ";\n";
    }
    return ret + "}\n";
    */
  }

}
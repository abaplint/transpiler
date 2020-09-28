import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {TranspileTypes} from "../types";

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
    ret += this.buildConstants(node.findFirstExpression(abaplint.Expressions.ClassName), traversal);

    return ret;
  }

///////////////////////////////

  private hasConstructor(node: abaplint.Nodes.StructureNode): boolean {
    for (const m of node.findAllStatements(abaplint.Statements.Method)) {
      const name = m.findFirstExpression(abaplint.Expressions.MethodName)?.getFirstToken().getStr();
      if (name?.toUpperCase() === "CONSTRUCTOR") {
        return true;
      }
    }
    return false;
  }

  private buildConstants(node: abaplint.Nodes.ExpressionNode | undefined, traversal: Traversal): string {
    if (node === undefined) {
      return "";
    }
    const scope = traversal.getSpaghetti().lookupPosition(node.getFirstToken().getStart(), traversal.getFilename());
    const vars = scope?.getData().vars;
    if (vars === undefined || vars.length === 0) {
      return "";
    }
    let ret = "";
    for (const v of vars) {
      if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.ReadOnly) === false) {
        continue;
      }
      const name = node.getFirstToken().getStr().toLowerCase() + "." + v.name;
      ret += name + " = " + new TranspileTypes().toType(v.identifier.getType()) + ";\n";
      ret += name + ".set(" + v.identifier.getValue() + ");\n";
    }
    return ret;
  }

  private buildConstructor(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const scope = traversal.getSpaghetti().lookupPosition(node.getFirstToken().getStart(), traversal.getFilename());

    const ret = traversal.buildConstructorContents(scope);
    if (ret === "") {
      return ret;
    }

    return "constructor() {\n" + ret + "}\n";
  }

}
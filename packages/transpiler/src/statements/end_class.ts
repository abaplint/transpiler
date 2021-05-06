import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";
import {IStatementTranspiler} from "./_statement_transpiler";

export class EndClassTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const def = traversal.getClassDefinition(node.getFirstToken());
    let ret = "}\n";
    // todo, this might cause local classes to spill to global scope
    ret += `abap.Classes['${def?.getName().toUpperCase()}'] = ${def?.getName().toLowerCase()};`;
    return ret;
  }

}
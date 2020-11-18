import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";
import {IStatementTranspiler} from "./_statement_transpiler";

export class CatchTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    let into = "";
    const intoNode = node.findExpressionAfterToken("INTO");
    if (intoNode) {
      into = "\n" + traversal.traverse(intoNode) + ".set(e);";
    }

    // todo, add more here
    return "} catch (e) {" + into;
  }

}
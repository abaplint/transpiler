import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";

export class ReturnTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    let extra = "";
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const vars = scope?.getData().vars;
    for (const n in vars) {
      const identifier = vars[n];
      if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        extra = " " + n.toLowerCase();
      }
    }

    let pre = "";
    if (traversal.isInsideDoOrWhile(node)) {
      pre = `abap.builtin.sy.get().index.set(${UniqueIdentifier.getIndexBackup1()});\n`;
    }

    return new Chunk().append(pre + "return" + extra + ";", node, traversal);
  }

}
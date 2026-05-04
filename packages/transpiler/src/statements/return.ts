import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";

export class ReturnTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const source = node.findDirectExpression(abaplint.Expressions.Source);

    let extra = "";
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const vars = scope?.getData().vars;
    for (const n in vars) {
      const identifier = vars[n];
      if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        extra = " " + Traversal.prefixVariable(n.toLowerCase());
      }
    }

    let pre = "";
    if (traversal.isInsideDoOrWhile(node)) {
      pre = `abap.builtin.sy.get().index.set(${UniqueIdentifier.getIndexBackup1()});\n`;
    }

    if (scope?.getIdentifier().stype === abaplint.ScopeType.Method
        && scope?.getIdentifier().sname.toLowerCase() === "constructor") {
      extra = " this";
    }

    // RETURN <source> assigns to the RETURNING parameter and exits immediately.
    if (source && extra !== " this") {
      return new Chunk().append(pre
        + extra.trimStart()
        + `.set(${traversal.traverse(source).getCode()});\nreturn`
        + extra
        + ";", node, traversal);
    }

    return new Chunk().append(pre + "return" + extra + ";", node, traversal);
  }

}
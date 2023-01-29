import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {UniqueIdentifier} from "../unique_identifier";
import {FieldChainTranspiler, SourceTranspiler} from "../expressions";
import {Chunk} from "../chunk";

export class LoopTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    if (!(node.get() instanceof abaplint.Statements.Loop)) {
      throw new Error("LoopTranspiler, unexpected node");
    }

    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.SimpleSource2)).getCode();

    const unique1 = UniqueIdentifier.get();
    let target = "";
    const into = node.findDirectExpression(abaplint.Expressions.LoopTarget)?.findDirectExpression(abaplint.Expressions.Target);
    if (into) {
      const concat = node.concatTokens().toUpperCase();
      const t = traversal.traverse(into).getCode();

      const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
      const typ = traversal.determineType(node, scope);
      if (concat.includes(" REFERENCE INTO ")) {
        // target is assumed to be a data reference
        target = t + ".assign(" + unique1 + ");";
      } else if (typ instanceof abaplint.BasicTypes.DataReference) {
        // row type and target is assumed to be data references
        target = t + ".assign(" + unique1 + ".getPointer());";
      } else {
        target = t + ".set(" + unique1 + ");";
      }
    } else {
      const assigning = node.findFirstExpression(abaplint.Expressions.FSTarget)?.findFirstExpression(abaplint.Expressions.FieldSymbol);
      if (assigning) {
        target = traversal.traverse(assigning).getCode() + ".assign(" + unique1 + ");";
      }
    }

    const extra: string[] = [];
    const fromNode = node.findExpressionAfterToken("FROM");
    if (fromNode) {
      const from = new SourceTranspiler().transpile(fromNode, traversal).getCode();
      extra.push("from: " + from);
    }

    const toNode = node.findExpressionAfterToken("TO");
    if (toNode) {
      const to = new SourceTranspiler().transpile(toNode, traversal).getCode();
      extra.push("to: " + to);
    }

    const keyNode = node.findExpressionAfterToken("KEY");
    if (keyNode) {
      if (keyNode.get() instanceof abaplint.Expressions.Dynamic) {
        const children = keyNode.getChildren();
        if (children[1] instanceof abaplint.Nodes.ExpressionNode
            && children[1].get() instanceof abaplint.Expressions.FieldChain) {
          const t = new FieldChainTranspiler(true).transpile(children[1], traversal);
          extra.push(`usingKey: ${t.getCode()}`);
        } else {
          extra.push(`usingKey: ${keyNode.concatTokens()}`);
        }
      } else {
        extra.push(`usingKey: "${keyNode.concatTokens()}"`);
      }
    }

    const whereNode = node.findDirectExpression(abaplint.Expressions.ComponentCond);
    if (whereNode) {
      const where = traversal.traverse(whereNode).getCode();
      // todo, evil workaround removing "await",
      extra.push("where: async " + where);
    }

    const topEquals: {[key: string]: string} = {};
    for (const compare of whereNode?.findDirectExpressions(abaplint.Expressions.ComponentCompare) || []) {
      const op = compare.findDirectExpression(abaplint.Expressions.CompareOperator)?.concatTokens().toUpperCase();
      if (op !== "=" && op !== "EQ") {
        continue;
      } else if (compare.findDirectTokenByText("NOT")) {
        continue;
      }
      const tchain = traversal.traverse(compare.findDirectExpression(abaplint.Expressions.ComponentChainSimple));
      const tsource = traversal.traverse(compare.findDirectExpression(abaplint.Expressions.Source));
      topEquals[tchain.getCode()] = tsource.getCode();
    }
    if (Object.keys(topEquals).length > 0) {
      const fields: string[] = [];
      for (const n in topEquals) {
        fields.push(`"${n}": ` + topEquals[n]);
      }
      extra.push("topEquals: {" + fields.join(",") + "}");
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }
    return new Chunk(`for await (const ${unique1} of abap.statements.loop(${source}${concat})) {\n${target}`);
  }

}
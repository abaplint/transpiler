import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AppendTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const concat = node.concatTokens().toUpperCase();

    const t = node.findDirectExpression(abaplint.Expressions.Target);
    let target: Chunk | undefined = undefined;
    if (t) {
      target = traversal.traverse(t);
    }

    if (concat.includes("INITIAL LINE") && target) {
      const assigning = node.findExpressionAfterToken("ASSIGNING");
      const into = node.findExpressionAfterToken("INTO");
      if (assigning) {
        const found = assigning.findFirstExpression(abaplint.Expressions.FieldSymbol);
        const fs = traversal.traverse(found).getCode();
        return new Chunk(fs + ".assign(" + target.getCode() + ".appendInitial());");
      } else if (into){
        const ref = traversal.traverse(into).getCode();
        return new Chunk(ref + ".assign(" + target.getCode() + ".appendInitial());");
      } else {
        return new Chunk(target.getCode() + ".appendInitial();");
      }
    } else {
      const options: Chunk[] = [];

      const s = node.findDirectExpression(abaplint.Expressions.SimpleSource4)
        || node.findDirectExpression(abaplint.Expressions.Source);
      if (s) {
        const option = new Chunk().appendString("source: ");
        option.appendChunk(traversal.traverse(s));
        options.push(option);
      }

      const assigning = node.findExpressionAfterToken("ASSIGNING");
      if (assigning) {
        const option = new Chunk().appendString("assigning: ");
        option.appendChunk(traversal.traverse(assigning.findFirstExpression(abaplint.Expressions.FieldSymbol)));
        options.push(option);
      }

      const referenceInto = node.findExpressionAfterToken("INTO");
      if (referenceInto && referenceInto.get() instanceof abaplint.Expressions.Target) {
        options.push(new Chunk().appendString("referenceInto: " + traversal.traverse(referenceInto).getCode()));
      }

      const to = node.findExpressionAfterToken("TO");
      if (to && to.get() instanceof abaplint.Expressions.Source) {
        options.push(new Chunk().appendString("to: " + traversal.traverse(to).getCode()));
      }
      const from = node.findExpressionAfterToken("FROM");
      if (from) {
        options.push(new Chunk().appendString("from: " + traversal.traverse(from).getCode()));
      }

      if (concat.startsWith("APPEND LINES OF ")) {
        options.push(new Chunk().appendString("lines: true"));
      }

      if (target) {
        options.push(new Chunk().appendString("target: ").appendChunk(target));
      }

      const ret = new Chunk();
      ret.append("abap.statements.append({", node, traversal);
      ret.join(options);
      ret.append("});", node.getLastToken(), traversal);
      return ret;
    }

  }

}
import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AppendTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const concat = node.concatTokens().toUpperCase();

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    if (concat.includes("INITIAL LINE")) {
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

      const s = node.findDirectExpression(abaplint.Expressions.SimpleSource4);
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

      options.push(new Chunk().appendString("target: ").appendChunk(target));

      const ret = new Chunk();
      ret.append("abap.statements.append({", node, traversal);
      ret.join(options);
      ret.append("});", node.getLastToken(), traversal);
      return ret;
    }

  }

}
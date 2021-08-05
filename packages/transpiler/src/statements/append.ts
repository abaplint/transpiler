import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AppendTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const concat = node.concatTokens();

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    if (concat.toUpperCase().includes("INITIAL LINE")) {
      const found = node.findFirstExpression(abaplint.Expressions.FieldSymbol);
      if (found) {
        const fs = traversal.traverse(found).getCode();
        return new Chunk(fs + ".assign(" + target.getCode() + ".appendInitial());");
      } else {
        const into = node.findExpressionAfterToken("INTO");
        const ref = traversal.traverse(into).getCode();
        return new Chunk(ref + ".assign(" + target.getCode() + ".appendInitial());");
      }
    } else {
      const options: Chunk[] = [];

      const s = node.findDirectExpression(abaplint.Expressions.SimpleSource4);
      if (s) {
        const option = new Chunk().appendString("source: ");
        option.appendChunk(new SourceTranspiler().transpile(s, traversal));
        options.push(option);
      }

      const assigning = node.findExpressionAfterToken("ASSIGNING");
      if (assigning) {
        const option = new Chunk().appendString("assigning: ");
        option.appendChunk(traversal.traverse(assigning.findFirstExpression(abaplint.Expressions.FieldSymbol)));
        options.push(option);
      }

      if (concat.startsWith("APPEND LINES OF ")) {
        options.push(new Chunk().appendString("lines: true"));
      }

      options.push(new Chunk().appendString("target: ").appendChunk(target));

      const ret = new Chunk();
      ret.appendString("abap.statements.append({");
      ret.join(options);
      ret.appendString("});");
      return ret;
    }

  }

}
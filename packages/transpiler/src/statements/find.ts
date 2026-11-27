import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class FindTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const options: string[] = [];
    let index = 1;

    const sources = node.findDirectExpressions(abaplint.Expressions.Source);
    const source0 = traversal.traverse(sources[0]).getCode();
    if (node.findFirstExpression(abaplint.Expressions.FindType)?.findDirectTokenByText("REGEX")) {
      options.push("regex: " + source0);
    } else if (node.findFirstExpression(abaplint.Expressions.FindType)?.findDirectTokenByText("PCRE")) {
      options.push("pcre: " + source0);
    } else {
      options.push("find: " + source0);
    }

    const concat = node.concatTokens().toUpperCase();
    if (concat.startsWith("FIND FIRST OCCURRENCE OF ")) {
      options.push("first: true");
    } else if (concat.startsWith("FIND ALL OCCURRENCES OF ")) {
      options.push("first: false");
    }

    if (concat.includes(" IGNORING CASE")) {
      options.push("ignoringCase: true");
    }

    if (concat.includes(" IN BYTE MODE")) {
      options.push("byteMode: true");
    }

    // IN SECTION [OFFSET off] [LENGTH len] OF dobj: the section sources come before the data object,
    // and LENGTH followed by a Source is the section length, MATCH LENGTH is followed by a Target
    const inSection = concat.includes(" IN SECTION ");
    let prev = undefined;
    let off: abaplint.Nodes.ExpressionNode | abaplint.Nodes.TokenNode | undefined;
    let len: abaplint.Nodes.ExpressionNode | abaplint.Nodes.TokenNode | undefined;
    for (const c of node.getChildren()) {
      const prevToken = prev?.getFirstToken().getStr().toUpperCase();
      if (inSection && prevToken === "OFFSET" && c.get() instanceof abaplint.Expressions.Source) {
        options.push("sectionOffset: " + traversal.traverse(c).getCode());
        index++;
      } else if (inSection && prevToken === "LENGTH" && c.get() instanceof abaplint.Expressions.Source) {
        options.push("sectionLength: " + traversal.traverse(c).getCode());
        index++;
      } else if (prevToken === "OFFSET" && c.get() instanceof abaplint.Expressions.Target && off === undefined) {
        options.push("offset: " + traversal.traverse(c).getCode());
        off = c;
      } else if (prevToken === "LENGTH" && c.get() instanceof abaplint.Expressions.Target && len === undefined) {
        len = c;
      }
      prev = c;
    }

    const source1 = traversal.traverse(sources[index]).getCode();

    const cnt = node.findExpressionAfterToken("COUNT");
    if (cnt) {
      options.push("count: " + traversal.traverse(cnt).getCode());
    }

    if (len) {
      options.push("length: " + traversal.traverse(len).getCode());
    }

    const res = node.findExpressionAfterToken("RESULTS");
    if (res) {
      options.push("results: " + traversal.traverse(res).getCode());
    }

    const firstSubmatch = node.findExpressionAfterToken("SUBMATCHES");
    if (firstSubmatch) {
      const submatches: string[] = [];
      for (const t of node.findDirectExpressions(abaplint.Expressions.Target)) {
        if (t === len || t === cnt || t === off) {
          continue;
        }
        submatches.push(traversal.traverse(t).getCode());
      }
      options.push("submatches: [" + submatches.join(",") + "]");
    }

    return new Chunk("abap.statements.find(" + source1 + ", {" + options.join(", ") + "});");
  }

}
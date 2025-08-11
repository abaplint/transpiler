import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {ComponentChainSimpleTranspiler, FieldSymbolTranspiler, SourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {Chunk} from "../chunk";

export class ReadTableTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    let prefix = "";
    const s = node.findDirectExpression(abaplint.Expressions.SimpleSource2);
    const ret = traversal.traverse(s).getCode();

    const extra: string[] = [];
    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      const s = new SourceTranspiler().transpile(index, traversal).getCode();
      extra.push("index: " + s);
    }

    const from = node.findExpressionAfterToken("FROM");
    if (from) {
      const s = new SourceTranspiler().transpile(from, traversal).getCode();
      extra.push("from: " + s);
    }

    const keyName = node.findExpressionAfterToken("KEY");
    if (keyName && node.findDirectTokenByText("COMPONENTS")) {
      extra.push("keyName: \"" + keyName.concatTokens() + "\"");
    }

    const binary = node.findTokenSequencePosition("BINARY", "SEARCH");
    if (binary) {
      extra.push("binarySearch: true");
    }
    const withTableKey = node.findTokenSequencePosition("WITH", "TABLE");
    if (withTableKey) {
      extra.push("withTableKey: true");
    }

    const rt = node.findDirectExpression(abaplint.Expressions.ReadTableTarget);
    const target = rt?.findDirectExpression(abaplint.Expressions.Target);
    let fs = rt?.findDirectExpression(abaplint.Expressions.FSTarget);
    if (rt && fs) {
      if (fs?.getFirstChild()?.get() instanceof abaplint.Expressions.InlineFS) {
        fs = fs.findFirstExpression(abaplint.Expressions.TargetFieldSymbol)!;
      }
      const name = new FieldSymbolTranspiler().transpile(fs, traversal).getCode();
      extra.push("assigning: " + name);
    } else if (target) {
      const name = traversal.traverse(target).getCode();
      if (rt?.findDirectTokenByText("REFERENCE")) {
        extra.push("referenceInto: " + name);
      } else {
        extra.push("into: " + name);
      }
    }

    const compare = node.findDirectExpression(abaplint.Expressions.ComponentCompareSimple);
    if (compare) {
      const withKey: string[] = [];
      const withKeyValue: string[] = [];
      const withKeySimple: string[] = [];
      const count = compare.getChildren().length / 3;
      let usesTableLine = false;
      for (let i = 0; i < count; i++) {
        const left = compare.getChildren()[i * 3];
        const source = compare.getChildren()[(i * 3) + 2];

        const s = traversal.traverse(source).getCode();

        let field = "";
        if (left.get() instanceof abaplint.Expressions.Dynamic
            && left instanceof abaplint.Nodes.ExpressionNode) {
          const concat = left.concatTokens().toLowerCase();
          field = "i." + concat.substring(2, concat.length - 2);
        } else if (left.get() instanceof abaplint.Expressions.ComponentChainSimple
            && left instanceof abaplint.Nodes.ExpressionNode) {
          field = new ComponentChainSimpleTranspiler("i.").transpile(left, traversal).getCode();
        } else {
          throw new Error("transpiler: READ TABLE, unexpected node");
        }
        if (field === "i.table_line") {
          usesTableLine = true;
        }

        if (s.includes("await")) {
          const id = UniqueIdentifier.get();
          prefix += "const " + id + " = " + s + ";\n";
          withKey.push("abap.compare.eq(" + field + ", " + id + ")");
          withKeyValue.push(`{key: (i) => {return ${field}}, value: ${id}}`);
          withKeySimple.push(`"${field.replace("i.", "").replace(/\$/g, "/")}": ${id}`);
        } else {
          withKey.push("abap.compare.eq(" + field + ", " + s + ")");
          withKeyValue.push(`{key: (i) => {return ${field}}, value: ${s}}`);
          withKeySimple.push(`"${field.replace("i.", "").replace(/\$/g, "/")}": ${s}`);
        }
      }
      extra.push("withKey: (i) => {return " + withKey.join(" && ") + ";}");
      extra.push(`withKeyValue: [${withKeyValue.join(",")}]`);
      extra.push(`usesTableLine: ${usesTableLine}`);
      extra.push(`withKeySimple: {${withKeySimple.join(",")}}`);
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",\n  ") + "}";
    }

    return new Chunk()
      .append(prefix + "abap.statements.readTable(", node, traversal)
      .appendString(ret + concat)
      .append(");", node.getLastToken(), traversal);
  }

}
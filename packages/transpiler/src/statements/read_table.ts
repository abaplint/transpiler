import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {ComponentChainSimpleTranspiler, FieldChainTranspiler, FieldSymbolTranspiler, SourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {Chunk} from "../chunk";

export class ReadTableTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    let prefix = "";
    const s = node.getChildren()[2];
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

    // COMPARING comp1 comp2 ... | ALL FIELDS, TRANSPORTING comp1 comp2 ...
    const children = node.getChildren();
    const comparingAt = children.findIndex(c => c instanceof abaplint.Nodes.TokenNode && c.concatTokens().toUpperCase() === "COMPARING");
    let transported: string[] | undefined = undefined;
    if (comparingAt >= 0) {
      const names: string[] = [];
      for (let i = comparingAt + 1; i < children.length; i++) {
        const c = children[i];
        if (!(c instanceof abaplint.Nodes.ExpressionNode) || !(c.get() instanceof abaplint.Expressions.FieldSub)) {
          break;
        }
        names.push(c.concatTokens().toLowerCase());
      }
      // the parser can read "COMPARING a TRANSPORTING b" as four compared names
      const cut = names.indexOf("transporting");
      if (cut >= 0) {
        transported = names.slice(cut + 1);
        names.splice(cut);
      }
      extra.push("comparing: " + JSON.stringify(names.join(" ") === "all fields" ? ["*"] : names));
    }
    const transportingFields = node.findDirectExpression(abaplint.Expressions.TransportingFields);
    if (transportingFields) {
      transported = transportingFields.findAllExpressions(abaplint.Expressions.FieldSub).map(f => f.concatTokens().toLowerCase());
    }
    if (transported !== undefined && transported.join(" ") !== "all fields") {
      extra.push("transporting: " + JSON.stringify(transported));
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
        let key = left.concatTokens().toLowerCase();
        // the component name is a JS object key, computed at runtime for a dynamic variable
        let keyExpression = "";
        if (left.get() instanceof abaplint.Expressions.Dynamic
            && left instanceof abaplint.Nodes.ExpressionNode) {
          const chain = left.findDirectExpression(abaplint.Expressions.FieldChain);
          if (chain) {
            // WITH TABLE KEY (variable) = value
            const name = new FieldChainTranspiler(true).transpile(chain, traversal).getCode();
            keyExpression = name + ".trimEnd().toLowerCase()";
            field = "i[" + keyExpression + "]";
          } else {
            // WITH TABLE KEY ('LITERAL') = value
            key = key.substring(2, key.length - 2);
            field = "i." + key;
          }
        } else if (left.get() instanceof abaplint.Expressions.ComponentChainSimple
            && left instanceof abaplint.Nodes.ExpressionNode) {
          field = new ComponentChainSimpleTranspiler("i.").transpile(left, traversal).getCode();
        } else {
          throw new Error("transpiler: READ TABLE, unexpected node");
        }
        if (field === "i.table_line") {
          usesTableLine = true;
        }

        const simpleKey = keyExpression !== "" ? "[" + keyExpression + "]" : JSON.stringify(key);
        // the operand is converted to the type of the component before the comparison, once per READ
        const id = UniqueIdentifier.get();
        if (key.includes("->")) {
          prefix += "const " + id + " = " + s + ";\n";
        } else {
          const component = keyExpression !== "" ? keyExpression : JSON.stringify(key);
          prefix += "const " + id + " = abap.statements.readTableKey(" + ret + ", " + component + ", " + s + ");\n";
        }
        withKey.push("abap.compare.eq(" + field + ", " + id + ")");
        withKeyValue.push(`{key: (i) => {return ${field}}, value: ${id}}`);
        withKeySimple.push(`${simpleKey}: ${id}`);
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

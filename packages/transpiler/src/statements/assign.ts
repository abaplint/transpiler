/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {FieldChainTranspiler, FieldSymbolTranspiler, SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AssignTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const assignSource = node.findDirectExpression(abaplint.Expressions.AssignSource);

    const sources = assignSource?.findDirectExpressionsMulti([abaplint.Expressions.Source,abaplint.Expressions.SimpleSource3]).map(
      e => new SourceTranspiler(false).transpile(e, traversal).getCode()) || [];
    const fs = new FieldSymbolTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.FSTarget)!, traversal).getCode();

    const options: string[] = [];

    const concat = node.concatTokens().toUpperCase();
    if (concat.startsWith("ASSIGN COMPONENT ")) {
      options.push("component: " + sources.shift());
    }

    options.push("target: " + fs);
    if (sources.length !== 0
        && assignSource?.findDirectExpression(abaplint.Expressions.Dynamic) === undefined) {
      options.push("source: " + sources.pop());
    } else {

      let dynamicName = "";
      for (const c of assignSource?.getChildren() || []) {
        if (c instanceof abaplint.Nodes.ExpressionNode
            && c.get() instanceof abaplint.Expressions.Dynamic
            && c.findFirstExpression(abaplint.Expressions.ConstantString)) {
          if (dynamicName !== "") {
            dynamicName += " + ";
          }
          dynamicName += c.findFirstExpression(abaplint.Expressions.ConstantString)?.getFirstToken().getStr();
        } else if (c instanceof abaplint.Nodes.ExpressionNode
            && c.get() instanceof abaplint.Expressions.Dynamic
            && c.findDirectExpression(abaplint.Expressions.FieldChain)) {
          if (dynamicName !== "") {
            dynamicName += " + ";
          }
          dynamicName +=  new FieldChainTranspiler(true).transpile(c.findDirectExpression(abaplint.Expressions.FieldChain) as abaplint.Nodes.ExpressionNode, traversal).getCode();
        } else if (c.concatTokens() === "(" || c.concatTokens() === ")") {
          continue;
        } else if (c.concatTokens() === "=>" || c.concatTokens() === "->") {
          dynamicName += " + '" + c.concatTokens() + "'";
        } else {
          if (dynamicName !== "") {
            dynamicName += " + ";
          }
          dynamicName += "'" + c.concatTokens() + "'";
        }
      }
      options.push(`dynamicName: ` + dynamicName);

      // dynamicSource is the first part of the dynamic part
      const first = assignSource?.getFirstChild();
      if (first?.get() instanceof abaplint.Expressions.Dynamic && first instanceof abaplint.Nodes.ExpressionNode) {
        const firstFirst = first.getChildren()[1];
        if (firstFirst?.get() instanceof abaplint.Expressions.Constant) {
          const s = firstFirst.getFirstToken().getStr().toLowerCase().match(/\w+/)?.toString();
          options.push(`dynamicSource: (() => {
            try { return ${s}; } catch {}
            try { return this.${s}; } catch {}
          })()`);
        } else if (firstFirst?.get() instanceof abaplint.Expressions.FieldChain && firstFirst instanceof abaplint.Nodes.ExpressionNode) {
          const code = new FieldChainTranspiler(true).transpile(firstFirst, traversal).getCode();
          options.push(`dynamicSource: (() => {
            const name = ${code}.toLowerCase().replace(/[~\\/]/g, "$").match(/[\\w\\$\\/]+/)[0];
            try { return eval(name); } catch {}
            try { return eval("this." + name); } catch {}
          })()`);
        }
      } else if (first?.get() instanceof abaplint.Expressions.Source && first instanceof abaplint.Nodes.ExpressionNode) {
//        const name = first.concatTokens().toLowerCase();
        const name = new SourceTranspiler().transpile(first, traversal).getCode();
        options.push(`dynamicSource: (() => {
          try { return ${name}; } catch {}
          try { return this.${name}; } catch {}
        })()`);
      }
    }

    if (concat.endsWith(" CASTING.") || concat.includes(" CASTING TYPE ")) {
      options.push("casting: true");
    }

    return new Chunk().append("abap.statements.assign({", node, traversal)
      .appendString(options.join(", "))
      .append("});", node.getLastToken(), traversal);
  }

}
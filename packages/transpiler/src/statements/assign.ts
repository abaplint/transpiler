/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {FieldChainTranspiler, FieldSymbolTranspiler, SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AssignTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const assignSource = node.findDirectExpression(abaplint.Expressions.AssignSource);
    const sources = assignSource?.findDirectExpressions(abaplint.Expressions.Source).map(
      e => new SourceTranspiler(false).transpile(e, traversal).getCode()) || [];
    const fs = new FieldSymbolTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.FSTarget)!, traversal).getCode();

    const options: string[] = [];

    const concat = node.concatTokens().toUpperCase();
    if (concat.startsWith("ASSIGN COMPONENT ")) {
      options.push("component: " + sources.shift());
    }

    options.push("target: " + fs);
    if (sources.length !== 0) {
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
        }
      }
      options.push(`dynamicName: ` + dynamicName);

      // dynamicSource is the first part only
      let dynamic = assignSource?.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.ConstantString);
      if (dynamic) {
        const s = dynamic.getFirstToken().getStr().toLowerCase().match(/\w+/);
        options.push(`dynamicSource: (() => {try { return ${s}; } catch {}})()`);
      } else {
        dynamic = assignSource?.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.FieldChain);
        if (dynamic) {
          const code = new FieldChainTranspiler(true).transpile(dynamic, traversal).getCode();
          options.push(`dynamicSource: (() => {try { return eval(${code}.toLowerCase().match(/\\w+/)[0]); } catch {}})()`);
        }
      }
    }

    if (concat.endsWith(" CASTING.")) {
      options.push("casting: true");
    }

    return new Chunk().append("abap.statements.assign({", node, traversal)
      .appendString(options.join(", "))
      .append("});", node.getLastToken(), traversal);
  }

}
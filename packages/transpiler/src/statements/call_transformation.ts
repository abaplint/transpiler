import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CallTransformationTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const lookup = traversal.lookupClassOrInterface("KERNEL_CALL_TRANSFORMATION", node.getFirstToken());

    const options: string[] = [];

    const name = node.findDirectExpression(abaplint.Expressions.NamespaceSimpleName)?.concatTokens();
    options.push("name: \"" + name + "\"");

    const sourceXML = node.findDirectExpression(abaplint.Expressions.Source);
    if (sourceXML) {
      options.push("sourceXML: " + traversal.traverse(sourceXML).getCode());
    }

    const resultXML = node.findDirectExpression(abaplint.Expressions.Target);
    if (resultXML) {
      options.push("resultXML: " + traversal.traverse(resultXML).getCode());
    }

    const callOptions = node.findExpressionAfterToken("OPTIONS");
    if (callOptions) {
      options.push("options: " + traversal.traverse(callOptions).getCode());
    }

    const result = node.findExpressionAfterToken("RESULT");
    if (result && result.get() instanceof abaplint.Expressions.CallTransformationParameters) {
      options.push("result: " + traversal.traverse(result).getCode());
    }

    const source = node.findExpressionAfterToken("Source");
    if (source && source.get() instanceof abaplint.Expressions.CallTransformationParameters) {
      options.push("source: " + traversal.traverse(source).getCode());
    }

    const call = `await ${lookup}.call({${options.join(",")}});`;

    return new Chunk().append(
      `if (${lookup} === undefined) throw new Error("CallTransformation, kernel class missing");\n${call}`, node, traversal);
  }

}
import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CallKernelTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const lookup = traversal.lookupClassOrInterface("KERNEL_CALL", node.getFirstToken());

    const options: string[] = [];

    const name = traversal.traverse(node.getChildren()[1]);
    options.push("name: " + name.getCode());

    for (const id of node.findDirectExpressions(abaplint.Expressions.KernelId)) {
      console.dir(id);
    }

    const call = `await ${lookup}.call({${options.join(",")}});`;

    return new Chunk().append(
      `if (${lookup} === undefined) throw new Error("Call kernel class missing");\n${call}`, node, traversal);
  }

}
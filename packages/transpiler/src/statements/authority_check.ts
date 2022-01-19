import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AuthorityCheckTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const lookup = traversal.lookupClassOrInterface("KERNEL_AUTHORITY_CHECK", node.getFirstToken());

    const options: string[] = [];
// todo
    const call = `await ${lookup}.call({${options.join(",")}});`;

    return new Chunk().append(
      `if (${lookup} === undefined) throw new Error("AuthorityCheck, kernel class missing");\n${call}`, node, traversal);
  }

}
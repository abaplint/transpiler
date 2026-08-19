import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AuthorityCheckTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const lookup = traversal.lookupClassOrInterface("KERNEL_AUTHORITY_CHECK", node.getFirstToken());
    const options: string[] = [];

    const object = node.findExpressionAfterToken("OBJECT");
    if (object) {
      options.push("object: " + traversal.traverse(object).getCode());
    }

    const user = node.findExpressionAfterToken("USER");
    if (user) {
      options.push("user: " + traversal.traverse(user).getCode());
    }

    const fields: string[] = [];
    const children = node.getChildren();
    for (let i = 0; i < children.length; i++) {
      if (children[i].concatTokens().toUpperCase() !== "ID") {
        continue;
      }

      const id = children[i + 1];
      const addition = children[i + 2]?.concatTokens().toUpperCase();
      if (id === undefined) {
        continue;
      }

      const field = ["id: " + traversal.traverse(id).getCode()];
      if (addition === "FIELD" && children[i + 3] !== undefined) {
        field.push("field: " + traversal.traverse(children[i + 3]).getCode());
      } else if (addition === "DUMMY") {
        field.push("dummy: true");
      }
      fields.push("{" + field.join(", ") + "}");
    }
    options.push("fields: [" + fields.join(", ") + "]");

    const call = `await ${lookup}.call({${options.join(",")}});`;

    return new Chunk().append(
      `if (${lookup} === undefined) throw new Error("AuthorityCheck, kernel class missing");\n${call}`, node, traversal);
  }
}

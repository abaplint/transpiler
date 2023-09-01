// import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SQLFieldNameTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): Chunk {
    const chunk = new Chunk();

    let concat = node.concatTokens();
    /*
    if (concat.includes("~") && concat.split("~")[0].includes("/")) {
      concat = "'" + concat.replace("~", "'~");
    } else {
      */
    concat = concat.replace(/~/, `\\".\\"`);
    concat = `\\"` + concat + `\\"`;
//    }

    chunk.appendString(concat);

    return chunk;
  }

}
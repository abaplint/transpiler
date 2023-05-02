import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {DataTranspiler} from "./data.js";
import {Chunk} from "../chunk.js";

export class ConstantTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    return new DataTranspiler().transpile(node, traversal);
  }

}
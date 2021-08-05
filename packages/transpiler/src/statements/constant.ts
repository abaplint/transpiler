import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {DataTranspiler} from "./data";
import {Chunk} from "../chunk";

export class ConstantTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    return new DataTranspiler().transpile(node, traversal);
  }

}
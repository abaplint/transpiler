import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {SourceTranspiler} from "./source.js";
import {Chunk} from "../chunk.js";

export class SimpleSource3Transpiler implements IExpressionTranspiler {
  private readonly addGet: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new SourceTranspiler(this.addGet).transpile(node, traversal);
  }
}
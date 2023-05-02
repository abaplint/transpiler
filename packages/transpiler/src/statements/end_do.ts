import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";
import {IStatementTranspiler} from "./_statement_transpiler.js";

export class EndDoTranspiler implements IStatementTranspiler {
  private readonly syIndexBackup: string;

  public constructor(syIndexBackup: string) {
    this.syIndexBackup = syIndexBackup;
  }

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    return new Chunk().append(`}
abap.builtin.sy.get().index.set(${this.syIndexBackup});\n`, node, traversal);
  }

}
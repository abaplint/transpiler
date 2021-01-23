import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class GetReferenceTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): string {
    return `throw new Error("GetReference, transpiler todo");`;
  }

}
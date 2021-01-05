import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {DataTranspiler as DataStatementTranspiler} from "../statements";

export class DataTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    const begin = node.findDirectStatement(abaplint.Statements.DataBegin);
    if (begin === undefined) {
      return "";
    }

    return new DataStatementTranspiler().transpile(begin, traversal) + "\n";
  }

}
import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TypePoolsTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
    // not required for loading, https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abaptype-pools.htm
    return new Chunk(``);
  }

}
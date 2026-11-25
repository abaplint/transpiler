import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {KERNEL_RAP_EML, emitCalls} from "./_eml";

/** ROLLBACK ENTITIES, lowered to KERNEL_RAP_EML.rollback */
export class RollbackEntitiesTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const lookup = traversal.lookupClassOrInterface(KERNEL_RAP_EML, node.getFirstToken());
    return emitCalls("RollbackEntities", lookup, [`await ${lookup}.rollback();`], node, traversal);
  }

}

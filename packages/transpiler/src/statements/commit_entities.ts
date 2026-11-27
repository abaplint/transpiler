import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {EMLWalker, KERNEL_RAP_EML, abapString, emitCalls, unsupportedAddition} from "./_eml";

/** COMMIT ENTITIES [RESPONSE OF bdef FAILED f REPORTED r] [RESPONSES FAILED f REPORTED r]
 *
 * lowered to KERNEL_RAP_EML.commit with the behavior definition of
 * RESPONSE OF and the targets named; the kernel sets sy-subrc. Not
 * lowered: IN SIMULATION MODE. */
export class CommitEntitiesTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const unsupported = unsupportedAddition(node, ["SIMULATION"]);
    if (unsupported !== undefined) {
      return new Chunk(`throw new Error("CommitEntities ${unsupported}, not supported, transpiler");`);
    }

    const lookup = traversal.lookupClassOrInterface(KERNEL_RAP_EML, node.getFirstToken());
    const walker = new EMLWalker(node);
    const options: string[] = [];

    while (!walker.done()) {
      const keyword = walker.takeKeyword();
      switch (keyword) {
        case "OF":
          options.push(`bdef: ${abapString(walker.takeName())}`);
          break;
        case "FAILED":
        case "REPORTED":
          options.push(`${keyword.toLowerCase()}: ${traversal.traverse(walker.takeExpression()).getCode()}`);
          break;
        default:
          break;
      }
    }

    return emitCalls("CommitEntities", lookup, [`await ${lookup}.commit({${options.join(", ")}});`], node, traversal);
  }

}

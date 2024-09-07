import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class EnhancementPointTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
    // for now, do nothing, but leave a comment

    // ENHANCEMENT-POINT enh_id SPOTS spot1 spot2 ... [STATIC] [INCLUDE BOUND].
    let enhId: string = "";
    const spots: string[] = [];
    for (const tn of _node.getTokenNodes()) {
      if (tn instanceof abaplint.Nodes.TokenNodeRegex) {
        if (!enhId) {
          enhId = tn.get().getStr();
        } else {
          spots.push(tn.get().getStr());
        }
      }
    }

    // [Transpiler] EnhancementPoint not implemented. Ignoring 'enh_id' spots 'spot1', 'spot2' ...
    return new Chunk(`// [Transpiler] EnhancementPoint not implemented. Ignoring '${enhId}' spots '${ spots.join("', '") }'`);
  }

}
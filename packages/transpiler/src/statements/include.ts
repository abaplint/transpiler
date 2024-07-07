import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
// import {Rearranger} from "../rearranger";

export class IncludeTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
/*
    const includeName = node.findDirectExpression(abaplint.Expressions.IncludeName)?.concatTokens();
    if (includeName === undefined) {
      throw new Error("INCLUDE, IncludeName not found");
    }

    const obj = traversal.getCurrentObject();
    if (obj instanceof abaplint.Objects.FunctionGroup) {
      if (includeName.toUpperCase().endsWith("XX") === false) {
        const include = obj.getInclude(includeName);
        if (include === undefined) {
          throw new Error(`Include ${includeName} not found`);
        }

        const sub = new Traversal(traversal.getSpaghetti(), include, traversal.getCurrentObject(), traversal.reg, traversal.options);
        const rearranged = new Rearranger().run(obj.getType(), include.getStructure());
        const chunk = sub.traverse(rearranged);
        console.dir(chunk.getCode());
        return chunk;
      }
    }
*/
    // todo, this will not work
    return new Chunk("");
  }

}
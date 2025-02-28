import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {Rearranger} from "../rearranger";

export class IncludeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const includeName = node.findDirectExpression(abaplint.Expressions.IncludeName)?.concatTokens();
    if (includeName === undefined) {
      throw new Error("INCLUDE, IncludeName not found");
    }

    const obj = traversal.getCurrentObject();
    if (obj instanceof abaplint.Objects.FunctionGroup) {
      if (includeName.toUpperCase().endsWith("XX") === true) {
        const include = obj.getInclude(includeName);
        if (include === undefined) {
          throw new Error(`Include ${includeName} not found`);
        }

        const sub = new Traversal(traversal.getSpaghetti(), include, traversal.getCurrentObject(), traversal.reg, traversal.options);
        const rearranged = new Rearranger().run(obj.getType(), include.getStructure());
        const chunk = sub.traverse(rearranged);
//        console.dir(chunk.getCode());
        return chunk;
      } else {
        return new Chunk("");
      }
    }

    const include = traversal.reg.getObject("PROG", includeName) as abaplint.Objects.Program | undefined;
    if (include === undefined) {
      throw new Error(`Include ${includeName} not found`);
    }
    const abapFile = include.getMainABAPFile();
    if (abapFile === undefined) {
      throw new Error(`Include ${includeName}, no main abap file found`);
    }

    const sub = new Traversal(traversal.getSpaghetti(), abapFile, traversal.getCurrentObject(), traversal.reg, traversal.options);
    const rearranged = new Rearranger().run(obj.getType(), abapFile.getStructure());
    const chunk = sub.traverse(rearranged);
//        console.dir(chunk.getCode());
    return chunk;
  }

}
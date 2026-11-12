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
      if (includeName.toUpperCase().endsWith("XX") === false) {
        const include = obj.getInclude(includeName);
        if (include === undefined) {
          // as an include of a program that is not there
          return new Chunk("// transpiler TODO: INCLUDE " + includeName + "\n");
        }

        const sub = new Traversal(traversal.getSpaghetti(), include, traversal.getCurrentObject(), traversal.reg,
          traversal.options, traversal.typeFactories);
        const rearranged = new Rearranger().run(obj.getType(), include.getStructure());
        const chunk = sub.traverse(rearranged);
//        console.dir(chunk.getCode());
        return chunk;
      }
    }

    if (obj instanceof abaplint.Objects.Program) {
      const prog = traversal.reg.getObject("PROG", includeName) as abaplint.Objects.Program | undefined;
      const file = prog?.getMainABAPFile();
      if (file !== undefined) {
        // traversed with the spaghetti of the main program, the include can use its types and data
        const sub = new Traversal(traversal.getSpaghetti(), file, obj, traversal.reg,
          traversal.options, traversal.typeFactories);
        sub.programMain = traversal.programMain;
        const rearranged = new Rearranger().run(obj.getType(), file.getStructure());
        const chunk = new Chunk();
        if (traversal.options?.addFilenames === true) {
          chunk.appendString("// " + file.getFilename() + "\n");
        }
        return chunk.appendChunk(sub.traverse(rearranged));
      }
    }

    // todo, this will not work
    return new Chunk("// transpiler TODO: INCLUDE " + includeName + "\n");
  }

}

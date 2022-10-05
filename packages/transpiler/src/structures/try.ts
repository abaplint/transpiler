import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TryTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const catches = node.findDirectStructures(abaplint.Structures.Catch);
    const cleanup = node.findDirectStructures(abaplint.Structures.Cleanup);
    let catchCode: Chunk | undefined = this.buildCatchCode(catches, traversal);

    for (const c of node.getChildren()) {
      if (c.get() instanceof abaplint.Structures.Catch) {
        if (catchCode) {
          ret.appendChunk(catchCode);
        }
        catchCode = undefined;
      } else if (c.get() instanceof abaplint.Structures.Cleanup) {
        ret.appendString(`} finally {\n// Transpiler todo: CLEANUP ignored\n`);
      } else if (c.get() instanceof abaplint.Statements.Try
          || c.get() instanceof abaplint.Statements.EndTry) {
        if (catches.length === 0 && cleanup.length === 0) {
          continue;
        }
        ret.appendChunk(traversal.traverse(c));
      } else {
        ret.appendChunk(traversal.traverse(c));
      }
    }
    return ret;
  }

/////////////////////

  private buildCatchCode(nodes: abaplint.Nodes.StructureNode[], traversal: Traversal): Chunk {
    let ret = "";
    let first = true;

    if (nodes.length === 0) {
      return new Chunk(ret);
    }

    ret += `} catch (e) {\n`;

    for (const n of nodes) {
      const catchStatement = n.findDirectStatement(abaplint.Statements.Catch);
      if (catchStatement === undefined) {
        throw "TryTranspiler, unexpected structure";
      }
      const catchNames = catchStatement.findDirectExpressions(abaplint.Expressions.ClassName).map(
        e => traversal.lookupClassOrInterface(e.concatTokens(), e.getFirstToken()));
      ret += first ? "" : " else ";
      first = false;
      ret += "if (" + catchNames?.map(n => "e instanceof " + n).join(" || ") + ") {\n";

      const intoNode = catchStatement.findExpressionAfterToken("INTO");
      if (intoNode) {
        ret += traversal.traverse(intoNode).getCode() + ".set(e);\n";
      }

      const body = n.findDirectStructure(abaplint.Structures.Body);
      if (body) {
        ret += traversal.traverse(body).getCode();
      }

      ret += "}";
    }

    // "else" unhandled in this TRY-CATCH, or a javascript runtime error
    ret += ` else {\n` +
    `throw e;\n` +
    `}\n`;

    return new Chunk(ret);
  }

}
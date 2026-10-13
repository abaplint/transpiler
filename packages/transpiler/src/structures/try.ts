import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TryTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const catches = node.findDirectStructures(abaplint.Structures.Catch);
    const cleanup = node.findDirectStructure(abaplint.Structures.Cleanup);
    // the CATCH blocks and the CLEANUP block become one javascript catch
    let handlerCode: Chunk | undefined = this.buildHandlerCode(catches, cleanup, traversal);

    for (const c of node.getChildren()) {
      if (c.get() instanceof abaplint.Structures.Catch
          || c.get() instanceof abaplint.Structures.Cleanup) {
        if (handlerCode) {
          ret.appendChunk(handlerCode);
        }
        handlerCode = undefined;
      } else if (c.get() instanceof abaplint.Statements.Try
          || c.get() instanceof abaplint.Statements.EndTry) {
        if (catches.length === 0 && cleanup === undefined) {
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

  private buildHandlerCode(nodes: abaplint.Nodes.StructureNode[], cleanup: abaplint.Nodes.StructureNode | undefined,
                           traversal: Traversal): Chunk | undefined {
    let ret = "";
    let first = true;

    if (nodes.length === 0 && cleanup === undefined) {
      return undefined;
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
      ret += "if (" + catchNames?.map(n => "(" + n + " && e instanceof " + n + ")").join(" || ") + ") {\n";

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

    // unhandled in this TRY-CATCH, or a javascript runtime error
    const unhandled = this.buildCleanupCode(cleanup, traversal) + `throw e;\n`;
    ret += nodes.length > 0 ? ` else {\n` + unhandled + `}\n` : unhandled;

    return new Chunk(ret);
  }

  /** CLEANUP runs when a class based exception leaves the TRY block, not for an exception raised
   * in a CATCH block of the same TRY, and not for javascript runtime errors */
  private buildCleanupCode(cleanup: abaplint.Nodes.StructureNode | undefined, traversal: Traversal): string {
    if (cleanup === undefined) {
      return "";
    }
    const cxRoot = traversal.lookupClassOrInterface("CX_ROOT", cleanup.getFirstToken());
    let ret = `if (${cxRoot} && e instanceof ${cxRoot}) {\n`;

    const intoNode = cleanup.findDirectStatement(abaplint.Statements.Cleanup)?.findExpressionAfterToken("INTO");
    if (intoNode) {
      ret += traversal.traverse(intoNode).getCode() + ".set(e);\n";
    }

    const body = cleanup.findDirectStructure(abaplint.Structures.Body);
    if (body) {
      ret += traversal.traverse(body).getCode();
    }

    return ret + `}\n`;
  }

}

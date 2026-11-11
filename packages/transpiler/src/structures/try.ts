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

  // Built as chunks, not as strings: the bodies keep their source mappings, so a stack frame, a
  // source map or a debugger can name an ABAP line inside a CATCH or CLEANUP block
  private buildHandlerCode(nodes: abaplint.Nodes.StructureNode[], cleanup: abaplint.Nodes.StructureNode | undefined,
                           traversal: Traversal): Chunk | undefined {
    if (nodes.length === 0 && cleanup === undefined) {
      return undefined;
    }

    const ret = new Chunk();
    ret.appendString(`} catch (e) {\n`);

    let first = true;
    for (const n of nodes) {
      const catchStatement = n.findDirectStatement(abaplint.Statements.Catch);
      if (catchStatement === undefined) {
        throw "TryTranspiler, unexpected structure";
      }
      const catchNames = catchStatement.findDirectExpressions(abaplint.Expressions.ClassName).map(
        e => traversal.lookupClassOrInterface(e.concatTokens(), e.getFirstToken()));
      if (first === false) {
        ret.appendString(" else ");
      }
      first = false;
      ret.append("if (" + catchNames?.map(n => "(" + n + " && e instanceof " + n + ")").join(" || ") + ") {\n",
                 catchStatement, traversal);

      const intoNode = catchStatement.findExpressionAfterToken("INTO");
      if (intoNode) {
        ret.appendChunk(traversal.traverse(intoNode)).appendString(".set(e);\n");
      }

      const body = n.findDirectStructure(abaplint.Structures.Body);
      if (body) {
        ret.appendChunk(traversal.traverse(body));
      }

      ret.appendString("}");
    }

    // unhandled in this TRY-CATCH, or a javascript runtime error
    if (nodes.length > 0) {
      ret.appendString(` else {\n`);
    }
    this.appendCleanupCode(ret, cleanup, traversal);
    ret.appendString(`throw e;\n`);
    if (nodes.length > 0) {
      ret.appendString(`}\n`);
    }

    return ret;
  }

  /** CLEANUP runs when a class based exception leaves the TRY block, not for an exception raised
   * in a CATCH block of the same TRY, and not for javascript runtime errors */
  private appendCleanupCode(ret: Chunk, cleanup: abaplint.Nodes.StructureNode | undefined, traversal: Traversal): void {
    if (cleanup === undefined) {
      return;
    }
    const cxRoot = traversal.lookupClassOrInterface("CX_ROOT", cleanup.getFirstToken());
    const cleanupStatement = cleanup.findDirectStatement(abaplint.Statements.Cleanup);
    const head = `if (${cxRoot} && e instanceof ${cxRoot}) {\n`;
    if (cleanupStatement) {
      ret.append(head, cleanupStatement, traversal);
    } else {
      ret.appendString(head);
    }

    const intoNode = cleanupStatement?.findExpressionAfterToken("INTO");
    if (intoNode) {
      ret.appendChunk(traversal.traverse(intoNode)).appendString(".set(e);\n");
    }

    const body = cleanup.findDirectStructure(abaplint.Structures.Body);
    if (body) {
      ret.appendChunk(traversal.traverse(body));
    }

    ret.appendString(`}\n`);
  }

}

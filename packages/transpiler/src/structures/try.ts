import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TryTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    let ret = "";

    const catches = node.findDirectStructures(abaplint.Structures.Catch);
    let catchCode = this.buildCatchCode(catches, traversal);

    for (const c of node.getChildren()) {
      if (c.get() instanceof abaplint.Structures.Catch) {
        ret += catchCode;
        catchCode = "";
      } else {
        ret += traversal.traverse(c).getCode();
      }
    }
    return new Chunk(ret);
  }

  private buildCatchCode(nodes: abaplint.Nodes.StructureNode[], traversal: Traversal): string {
    let ret = "";
    let first = true;

    if (nodes.length === 0) {
      return ret;
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

    return ret;
  }

}
import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {DataTranspiler} from "../statements";
import {Chunk} from "../chunk";

export class ConstantsTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const begin = node.findDirectStatement(abaplint.Statements.ConstantBegin);
    if (begin === undefined) {
      throw "ConstantsTranspilerBegin";
    }
    const name = begin.findDirectExpression(abaplint.Expressions.DefinitionName)?.getFirstToken().getStr();
    if (name === undefined) {
      throw "ConstantsTranspilerName";
    }

    const ret = new Chunk();
    ret.appendChunk(new DataTranspiler().transpile(begin, traversal).ensureStartMapping(begin, traversal));
    ret.appendString("\n");
    ret.appendString(ConstantsTranspiler.handleValues(name, node, traversal));

    return ret;
  }

  public static handleValues(prefix: string, node: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    let ret = "";

    // todo: CONSTANTS BEGIN inside CONSTANTS BEGIN
    for (const c of node.findDirectStatements(abaplint.Statements.Constant)) {
      const field = c.findDirectExpression(abaplint.Expressions.DefinitionName)?.getFirstToken().getStr();
      if (field === undefined) {
        continue;
      }
      const value = c.findFirstExpression(abaplint.Expressions.Constant);
      if (value) {
        ret += `${prefix}.get().${field}.set(${traversal.traverse(value).getCode()});\n`;
      }
    }

    return ret;
  }

}
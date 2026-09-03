import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {AtFirstTranspiler} from "./at_first";
import {AtLastTranspiler} from "./at_last";
import {UniqueIdentifier} from "../unique_identifier";
import {LoopTranspiler as LoopStatementTranspiler} from "../statements";
import {AtTranspiler} from "./at";
import {InlineDeclarations} from "../inline";


export class LoopTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    let pre = "";
    let atFirst: Chunk | undefined = undefined;
    let atLast: Chunk | undefined = undefined;
    let atLastFlag = "";
    let loopStatement: abaplint.Nodes.StatementNode | undefined = undefined;
    let previous = "";
    let tabix = "";
    let loopTarget = "";

    let hasAt = false;
    for (const n of node.findDirectStructure(abaplint.Structures.Body)?.findDirectStructures(abaplint.Structures.Normal) || []) {
      if (n.findDirectStructure(abaplint.Structures.At) !== undefined) {
        hasAt = true;
        break;
      }
    }

    const hasAtLast = (node.findDirectStructure(abaplint.Structures.Body)
      ?.findDirectStructures(abaplint.Structures.Normal) || [])
      .some(n => n.findDirectStructure(abaplint.Structures.AtLast) !== undefined);
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    if (scope?.getIdentifier().stype === abaplint.ScopeType.Program
        || scope?.getIdentifier().stype === abaplint.ScopeType.SelectionEvent) {
      ret.appendString(InlineDeclarations.buildDeclarations(node, traversal));
    }
    if (hasAtLast) {
      atLastFlag = UniqueIdentifier.get();
      ret.appendString(`let ${atLastFlag} = false;\n`);
    }

    if (node.findDirectStatement(abaplint.Statements.Loop)?.findDirectTokenByText("GROUP") !== undefined) {
      return new Chunk(`throw new Error("transpiler todo, GROUP BY");`);
    }

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StructureNode && c.get() instanceof abaplint.Structures.Body) {
        for (const b of c.getChildren()) {
          for (const n of b.getChildren()) {
            if (n instanceof abaplint.Nodes.StructureNode && n.get() instanceof abaplint.Structures.AtFirst) {
              atFirst = new AtFirstTranspiler().transpile(n, traversal);
              const u = UniqueIdentifier.get();
              pre = "let " + u + " = false;\n";
              ret.appendString("if (" + u + " === false) {\n");
              ret.appendChunk(atFirst);
              ret.appendString(u + " = true;\n");
              ret.appendString("}\n");
            } else if (n instanceof abaplint.Nodes.StructureNode && n.get() instanceof abaplint.Structures.AtLast) {
              atLast = new AtLastTranspiler().transpile(n, traversal);
              ret.appendString(`if (${atLastFlag}) {\n`);
              ret.appendChunk(atLast);
              ret.appendString("}\n");
            } else if (n instanceof abaplint.Nodes.StructureNode && n.get() instanceof abaplint.Structures.At) {
              ret.appendChunk(new AtTranspiler().transpile(n, traversal, previous, loopTarget, tabix, loopStatement));
            } else {
              ret.appendChunk(traversal.traverse(n));
            }
          }
        }
      } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.Loop) {
        if (hasAt === true) {
          previous = UniqueIdentifier.get();
          tabix = UniqueIdentifier.get();
          ret.appendString(`let ${previous} = undefined;\n`);
          ret.appendString(`let ${tabix} = undefined;\n`);
        }
        const loop = new LoopStatementTranspiler({atLast: atLastFlag || undefined});
        // LoopStatementTranspiler maps its own head, so no ensureStartMapping needed here
        ret.appendChunk(loop.transpile(c, traversal));
        ret.appendString("\n");
        if (hasAt === true) {
          ret.appendString(tabix + " = new abap.types.Integer().set(abap.builtin.sy.get().tabix.get() + 1);\n");
        }
        loopTarget = loop.getTarget();
        loopStatement = c;
      } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.EndLoop) {
        if (hasAt === true) {
          ret.appendString(`${previous} = ${loopTarget};\n`);
        }
        ret.appendChunk(traversal.traverse(c));
      } else {
        ret.appendChunk(traversal.traverse(c));
      }
    }

    const atted = new Chunk();
    if (pre) {
      atted.appendString(pre);
    }

    atted.appendChunk(ret);

    return atted;
  }

}
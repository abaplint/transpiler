import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {ControlLevel, Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {AtFirstTranspiler} from "./at_first";
import {AtLastTranspiler} from "./at_last";
import {UniqueIdentifier} from "../unique_identifier";
import {LoopTranspiler as LoopStatementTranspiler} from "../statements";
import {AtTranspiler} from "./at";

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
    let level: ControlLevel | undefined = undefined;

    let hasAt = false;
    for (const n of node.findDirectStructure(abaplint.Structures.Body)?.findDirectStructures(abaplint.Structures.Normal) || []) {
      if (n.findDirectStructure(abaplint.Structures.At) !== undefined) {
        hasAt = true;
        break;
      }
    }
    const hasSum = node.findFirstStatement(abaplint.Statements.Sum) !== undefined;

    const hasAtLast = (node.findDirectStructure(abaplint.Structures.Body)
      ?.findDirectStructures(abaplint.Structures.Normal) || [])
      .some(n => n.findDirectStructure(abaplint.Structures.AtLast) !== undefined);
    if (hasAtLast) {
      atLastFlag = UniqueIdentifier.get();
      ret.appendString(`let ${atLastFlag} = false;\n`);
    }

    if (node.findDirectStatement(abaplint.Statements.Loop)?.findDirectTokenByText("GROUP") !== undefined) {
      return new Chunk(`throw new Error("transpiler todo, GROUP BY");`);
    }

    // AT FIRST and AT LAST: with INTO, every component of the work area shows "*" or its initial value inside the block
    const firstOrLast = (body: Chunk) => {
      const workArea = level?.workArea;
      const wrapped = new Chunk();
      if (workArea) {
        wrapped.appendString(`abap.statements.atMask(${workArea}, "");\n`);
      }
      wrapped.appendChunk(body);
      if (workArea) {
        wrapped.appendString(`${workArea}.set(${loopTarget});\n`);
      }
      return wrapped;
    };

    try {
      for (const c of node.getChildren()) {
        if (c instanceof abaplint.Nodes.StructureNode && c.get() instanceof abaplint.Structures.Body) {
          for (const b of c.getChildren()) {
            for (const n of b.getChildren()) {
              if (n instanceof abaplint.Nodes.StructureNode && n.get() instanceof abaplint.Structures.AtFirst) {
                atFirst = firstOrLast(new AtFirstTranspiler().transpile(n, traversal));
                const u = UniqueIdentifier.get();
                pre = "let " + u + " = false;\n";
                ret.appendString("if (" + u + " === false) {\n");
                ret.appendChunk(atFirst);
                ret.appendString(u + " = true;\n");
                ret.appendString("}\n");
              } else if (n instanceof abaplint.Nodes.StructureNode && n.get() instanceof abaplint.Structures.AtLast) {
                atLast = firstOrLast(new AtLastTranspiler().transpile(n, traversal));
                ret.appendString(`if (${atLastFlag}) {\n`);
                ret.appendChunk(atLast);
                ret.appendString("}\n");
              } else if (n instanceof abaplint.Nodes.StructureNode && n.get() instanceof abaplint.Structures.At) {
                ret.appendChunk(new AtTranspiler().transpile(n, traversal, previous, loopTarget, tabix, loopStatement, level));
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
          let rows = "";
          let collect = "";
          if (hasAt === true || hasSum === true) {
            // SUM adds up rows of the LOOP: they are collected once, when the first SUM runs
            rows = UniqueIdentifier.get();
            ret.appendString(`let ${rows} = undefined;\n`);
            const all = new LoopStatementTranspiler({skipInto: true});
            const head = all.transpile(c, traversal).getCode();
            const backup = UniqueIdentifier.get();
            collect = `if (${rows} === undefined) {\nconst ${backup} = abap.builtin.sy.get().tabix.get();\n${rows} = [];\n` +
              `${head}\n${rows}.push(${all.getTarget()});\n}\nabap.builtin.sy.get().tabix.set(${backup});\n}\n`;
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
          if (hasAt === true || hasSum === true) {
            level = {workArea: loop.getWorkArea(), assigning: loop.isAssigning(), row: loopTarget, rows, collect, key: undefined};
            traversal.controlLevels.push(level);
          }
        } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.EndLoop) {
          if (hasAt === true) {
            ret.appendString(`${previous} = ${loopTarget};\n`);
          }
          ret.appendChunk(traversal.traverse(c));
        } else {
          ret.appendChunk(traversal.traverse(c));
        }
      }
    } finally {
      if (level !== undefined) {
        traversal.controlLevels.pop();
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

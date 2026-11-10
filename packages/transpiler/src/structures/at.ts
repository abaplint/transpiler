import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {ControlLevel, Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {LoopTranspiler} from "../statements";
import {UniqueIdentifier} from "../unique_identifier";


export class AtTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode,
                   traversal: Traversal,
                   previous?: string,
                   loopTarget?: string,
                   tabix?: string,
                   loopStatement?: abaplint.Nodes.StatementNode,
                   level?: ControlLevel): Chunk {

    const ret = new Chunk();

    const atStatement = node.findDirectStatement(abaplint.Statements.At);
    const concat = atStatement?.concatTokens().toUpperCase();
    const key = atStatement ? AtTranspiler.keyCode(atStatement, traversal) : undefined;
    if (key === undefined) {
      return new Chunk(`throw new Error("AT with a field symbol, not supported, transpiler");`);
    }

    // the control key is the component and every component left of it
    const same = (a: string) => `abap.statements.atSameKey(${a}, ${loopTarget}, ${key})`;

    if (concat?.startsWith("AT NEW ")) {
      ret.appendString(`if (${previous} === undefined || ${same(previous!)} === false) {\n`);
      ret.appendChunk(AtTranspiler.body(node, traversal, key, loopTarget!, level));
      ret.appendString("}\n");
    } else if (concat?.startsWith("AT END OF ")) {
      const next = UniqueIdentifier.get();
      const tabixBackup = UniqueIdentifier.get();
      ret.appendString(`let ${next} = undefined;\n`);
      ret.appendString(`const ${tabixBackup} = abap.builtin.sy.get().tabix.get();\n`);
      const loop = new LoopTranspiler({injectFrom: tabix, skipInto: true});
      ret.appendChunk(loop.transpile(loopStatement!, traversal));
      ret.appendString(`${next} = ${loop.getTarget()};\n`);
      ret.appendString(`break;\n`);
      ret.appendString(`}\n`);
      // looking at the next row does not move sy-tabix
      ret.appendString(`abap.builtin.sy.get().tabix.set(${tabixBackup});\n`);
      ret.appendString(`if (${next} === undefined || ${same(next)} === false) {\n`);
      ret.appendChunk(AtTranspiler.body(node, traversal, key, loopTarget!, level));
      ret.appendString("}\n");
    }

    return ret;
  }

  /** the statements of an AT block; with LOOP ... INTO, the work area shows "*" right of the control key inside the
   * block (key "" for AT FIRST and AT LAST), and the row again after ENDAT */
  public static body(node: abaplint.Nodes.StructureNode, traversal: Traversal, key: string, row: string,
                     level: ControlLevel | undefined): Chunk {
    const ret = new Chunk();
    const workArea = level?.workArea;
    if (workArea) {
      ret.appendString(`abap.statements.atMask(${workArea}, ${key});\n`);
    }
    const before = level?.key;
    if (level) {
      level.key = key === `""` ? undefined : key;
    }
    try {
      const body = node.findDirectStructure(abaplint.Structures.Body);
      if (body) {
        ret.appendChunk(traversal.traverse(body));
      }
    } finally {
      if (level) {
        level.key = before;
      }
    }
    if (workArea) {
      ret.appendString(`${workArea}.set(${row});\n`);
    }
    return ret;
  }

  /** the control key as a JS expression giving the component name */
  private static keyCode(atStatement: abaplint.Nodes.StatementNode, traversal: Traversal): string | undefined {
    const sub = atStatement.findDirectExpression(abaplint.Expressions.FieldSub);
    if (sub) {
      return JSON.stringify(sub.concatTokens().toLowerCase());
    }
    const dynamic = atStatement.findDirectExpression(abaplint.Expressions.Dynamic);
    const inner = dynamic?.getChildren()[1];
    if (inner instanceof abaplint.Nodes.ExpressionNode) {
      if (inner.get() instanceof abaplint.Expressions.Constant) {
        return JSON.stringify(inner.concatTokens().replace(/^['`]|['`]$/g, "").toLowerCase());
      }
      return `${traversal.traverse(inner).getCode()}.get()`;
    }
    return undefined;
  }

}

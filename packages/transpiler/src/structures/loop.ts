import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {AtFirstTranspiler} from "./at_first";
import {AtLastTranspiler} from "./at_last";
import {UniqueIdentifier} from "../unique_identifier";
import {LoopTranspiler as LoopStatementTranspiler} from "../statements";
import {AtTranspiler} from "./at";
import {SourceTranspiler} from "../expressions";

/** the group loops being transpiled, innermost last: name is the group variable, or the row target
 *  for a group loop without group target, entry the JS name of the current group */
const groupLoops: {name: string, entry: string}[] = [];

export class LoopTranspiler implements IStructureTranspiler {

  /** the variable name of a Target or FSTarget, lower case, as LOOP AT GROUP refers to it */
  private static targetName(node: abaplint.Nodes.ExpressionNode | undefined): string | undefined {
    if (node === undefined) {
      return undefined;
    }
    const field = node.findFirstExpression(abaplint.Expressions.FieldSymbol)
      ?? node.findFirstExpression(abaplint.Expressions.TargetField)
      ?? node;
    return field.concatTokens().toLowerCase();
  }

  /** LOOP AT GROUP group: the JS name of the current group of the enclosing group loop, undefined for other loops */
  private static findGroupEntry(loopStatement: abaplint.Nodes.StatementNode): string | undefined {
    const at = loopStatement.getChildren()[2];
    if (!(at instanceof abaplint.Nodes.TokenNode) || at.concatTokens().toUpperCase() !== "GROUP") {
      return undefined;
    }
    const name = loopStatement.findDirectExpression(abaplint.Expressions.LoopSource)?.concatTokens().toLowerCase();
    const entry = [...groupLoops].reverse().find(g => g.name === name)?.entry;
    if (entry === undefined) {
      throw new Error("LOOP AT GROUP, no enclosing group loop for " + name);
    }
    return entry;
  }

  /** the group key: the code of a simple key, or a list of [component, value] for a structured key */
  private groupKey(groupBy: abaplint.Nodes.ExpressionNode, traversal: Traversal): string {
    const components = groupBy.findDirectExpressions(abaplint.Expressions.LoopGroupByComponent);
    if (components.length === 0) {
      const source = groupBy.findDirectExpression(abaplint.Expressions.Source);
      return new SourceTranspiler(false).transpile(source!, traversal).getCode();
    }
    const list: string[] = [];
    for (const c of components) {
      const compare = c.findDirectExpression(abaplint.Expressions.ComponentCompareSingle);
      if (compare !== undefined) {
        const name = compare.findDirectExpression(abaplint.Expressions.ComponentChainSimple)!.concatTokens().toLowerCase();
        const sourceNode = compare.findDirectExpression(abaplint.Expressions.Source)!;
        const source = new SourceTranspiler(false).transpile(sourceNode, traversal).getCode();
        list.push(`["${name}", ${source}]`);
      } else {
        const name = c.findDirectExpression(abaplint.Expressions.ComponentName)!.concatTokens().toLowerCase();
        const what = c.concatTokens().toUpperCase().endsWith("GROUP SIZE") ? "SIZE" : "INDEX";
        list.push(`["${name}", "${what}"]`);
      }
    }
    return "[" + list.join(", ") + "]";
  }

  /** LOOP AT itab ... GROUP BY key [ASCENDING|DESCENDING [AS TEXT]] [WITHOUT MEMBERS] [INTO|ASSIGNING|REFERENCE INTO group],
   *  first all rows are read and grouped, then the body runs once per group */
  private transpileGroupBy(node: abaplint.Nodes.StructureNode, loopStatement: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const groupBy = loopStatement.findDirectExpression(abaplint.Expressions.LoopGroupBy)!;

    const collector = UniqueIdentifier.get();
    const entry = UniqueIdentifier.get();
    const ret = new Chunk();

    // the grouping, every row is read into the row target, then its key is evaluated
    const statement = new LoopStatementTranspiler({groupEntry: LoopTranspiler.findGroupEntry(loopStatement)});
    ret.appendString(`const ${collector} = abap.statements.groupBy();\n`);
    ret.appendChunk(statement.transpile(loopStatement, traversal));
    ret.appendString(`\n${collector}.add(${statement.getTarget()}, ${this.groupKey(groupBy, traversal)});\n}\n`);

    let order = "";
    if (groupBy.findDirectTokenByText("ASCENDING") !== undefined) {
      order = `"ASCENDING"`;
    } else if (groupBy.findDirectTokenByText("DESCENDING") !== undefined) {
      order = `"DESCENDING"`;
    }
    if (order !== "" && groupBy.findDirectTokenByText("TEXT") !== undefined) {
      order += ", true";
    }
    // the group loop binds the group key, or without a group target the first row of each group
    const groupTarget = groupBy.findDirectExpression(abaplint.Expressions.LoopGroupByTarget);
    const pass = UniqueIdentifier.get();
    ret.append(`for (const [${pass}, ${entry}] of ${collector}.getGroups(${order}).entries()) {\n`, loopStatement, traversal);
    ret.appendString(`${collector}.setTabix(${entry}, ${pass} + 1, ${groupTarget === undefined});\n`);
    let name: string | undefined;
    if (groupTarget !== undefined) {
      const fs = groupTarget.findDirectExpression(abaplint.Expressions.FSTarget);
      const target = groupTarget.findDirectExpression(abaplint.Expressions.Target);
      if (fs !== undefined) {
        const symbol = fs.findFirstExpression(abaplint.Expressions.FieldSymbol)!;
        ret.appendString(`${traversal.traverse(symbol).getCode()}.assign(${entry}.key);\n`);
        name = LoopTranspiler.targetName(fs);
      } else {
        const field = target?.getFirstChild()?.get() instanceof abaplint.Expressions.InlineData
          ? target.findFirstExpression(abaplint.Expressions.TargetField)!
          : target!;
        const code = traversal.traverse(field).getCode();
        const reference = groupTarget.findDirectTokenByText("REFERENCE") !== undefined;
        ret.appendString(reference ? `${code}.assign(${entry}.key);\n` : `${code}.set(${entry}.key);\n`);
        name = LoopTranspiler.targetName(target);
      }
    } else {
      ret.appendString(`const ${statement.getTarget()} = ${entry}.representative;\n`);
      ret.appendString(statement.getTargetCode() + "\n");
      const rowTarget = loopStatement.findDirectExpression(abaplint.Expressions.LoopTarget);
      name = LoopTranspiler.targetName(rowTarget?.findDirectExpression(abaplint.Expressions.FSTarget)
        ?? rowTarget?.findDirectExpression(abaplint.Expressions.Target));
    }

    groupLoops.push({name: name ?? "", entry});
    try {
      for (const c of node.findDirectStructure(abaplint.Structures.Body)?.getChildren() || []) {
        ret.appendChunk(traversal.traverse(c));
      }
    } finally {
      groupLoops.pop();
    }

    const endLoop = node.findDirectStatement(abaplint.Statements.EndLoop);
    ret.append("}", endLoop ?? loopStatement, traversal);
    return ret;
  }

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const loopHead = node.findDirectStatement(abaplint.Statements.Loop);
    if (loopHead?.findDirectExpression(abaplint.Expressions.LoopGroupBy) !== undefined) {
      return this.transpileGroupBy(node, loopHead, traversal);
    }

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
    if (hasAtLast) {
      atLastFlag = UniqueIdentifier.get();
      ret.appendString(`let ${atLastFlag} = false;\n`);
    }

    const groupEntry = loopHead === undefined ? undefined : LoopTranspiler.findGroupEntry(loopHead);

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
        const loop = new LoopStatementTranspiler({atLast: atLastFlag || undefined, groupEntry});
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

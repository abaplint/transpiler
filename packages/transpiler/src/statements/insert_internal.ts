import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class InsertInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const options: string[] = [];

    const source = node.findDirectExpression(abaplint.Expressions.SimpleSource4)
      || node.findDirectExpression(abaplint.Expressions.Source);
    const target = node.findDirectExpression(abaplint.Expressions.Target);
    // INSERT TABLE itab. and INSERT itab INDEX n. name only the table, its header line goes into it
    const shortForm = target === undefined && source !== undefined ? traversal.traverse(source).getCode() : undefined;

    const concat = node.concatTokens().toUpperCase();
    if (concat.startsWith("INSERT LINES OF ")) {
      options.push("lines: true");
    }

    if (concat.startsWith("INSERT INITIAL LINE ")) {
      options.push("initial: true");
    } else if (shortForm !== undefined) {
      options.push("data: " + shortForm + ".getHeader()");
    } else {
      options.push("data: " + traversal.traverse(source).getCode());
    }

    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      options.push("index: " + traversal.traverse(index).getCode());
    } else if (!concat.startsWith("INSERT LINES OF ") && !concat.includes(" INTO TABLE ") && concat.includes(" INTO ")
        && node.findDirectExpression(abaplint.Expressions.Target) !== undefined) {
      // INSERT wa INTO itab: in a LOOP over itab, in front of the current row
      options.push("currentRow: true");
    }

    if (concat.includes(" REFERENCE INTO ")) {
      const targets = node.findDirectExpressions(abaplint.Expressions.Target);
      options.push("referenceInto: " + traversal.traverse(targets[1]).getCode());
    }

    const assigning = node.findExpressionAfterToken("ASSIGNING");
    if (assigning) {
      options.push("assigning: " + traversal.traverse((assigning.findFirstExpression(abaplint.Expressions.FieldSymbol))).getCode());
    }

    options.push("table: " + (shortForm ?? traversal.traverse(target).getCode()));

    return new Chunk()
      .append(`abap.statements.insertInternal({`, node, traversal)
      .appendString(options.join(", "))
      .append(`});`, node.getLastToken(), traversal);
  }

}
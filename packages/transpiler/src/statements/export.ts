import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {mediumOptions, sharedMedium} from "./_data_cluster";

export class ExportTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const medium = sharedMedium(node, "TO", traversal);
    const parameters = medium === undefined ? undefined : this.parameters(node, traversal);
    if (medium === undefined || parameters === undefined) {
      return new Chunk(`throw new Error("Export, transpiler todo");`);
    }
    return new Chunk(`abap.statements.exportCluster({${mediumOptions(medium)}, parameters: {${parameters.join(", ")}}});`);
  }

  // p1 = dobj1 p2 FROM dobj2 dobj3 ..., up to the TO: name -> value
  private parameters(node: abaplint.Nodes.StatementNode, traversal: Traversal): string[] | undefined {
    const children = node.getChildren();
    const ret: string[] = [];
    let i = 1;
    while (i < children.length) {
      const child = children[i];
      if (child instanceof abaplint.Nodes.TokenNode && child.getFirstToken().getStr().toUpperCase() === "TO") {
        break;
      }
      if (!(child instanceof abaplint.Nodes.ExpressionNode)) {
        return undefined;
      }
      const left = child.get();
      if (!(left instanceof abaplint.Expressions.FieldSub) && !(left instanceof abaplint.Expressions.FieldSymbol)) {
        return undefined; // a dynamic parameter list (ptab) is not transpiled
      }
      const name = child.concatTokens().toUpperCase();
      const sep = children[i + 1];
      const word = sep instanceof abaplint.Nodes.TokenNode ? sep.getFirstToken().getStr().toUpperCase() : "";
      if ((word === "=" || word === "FROM") && children[i + 2] instanceof abaplint.Nodes.ExpressionNode) {
        ret.push(`${JSON.stringify(name)}: ${traversal.traverse(children[i + 2]).getCode()}`);
        i = i + 3;
        continue;
      }
      // the short form: the parameter is the data object of that name
      if (left instanceof abaplint.Expressions.FieldSymbol) {
        ret.push(`${JSON.stringify(name)}: ${traversal.traverse(child).getCode()}`);
      } else if (child.getChildren().length === 1) {
        ret.push(`${JSON.stringify(name)}: ${traversal.prefixAndName(child.getFirstToken())}`);
      } else {
        return undefined;
      }
      i = i + 1;
    }
    return ret.length === 0 ? undefined : ret;
  }

}

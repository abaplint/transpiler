import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {mediumOptions, sharedMedium} from "./_data_cluster";

export class ImportTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const medium = sharedMedium(node, "FROM", traversal);
    const parameters = medium === undefined ? undefined : this.parameters(node, traversal);
    if (medium === undefined || parameters === undefined) {
      return new Chunk(`throw new Error("Import, transpiler todo");`);
    }
    return new Chunk(`abap.statements.importCluster({${mediumOptions(medium)}, parameters: {${parameters.join(", ")}}});`);
  }

  // p1 = dobj1 / p1 TO dobj1 / dobj1 ..., up to the FROM: name -> target
  // (abaplint also parses INTO in place of TO, read here the same way)
  private parameters(node: abaplint.Nodes.StatementNode, traversal: Traversal): string[] | undefined {
    const children = node.getChildren();
    const ret: string[] = [];
    let i = 1;
    while (i < children.length) {
      const child = children[i];
      if (child instanceof abaplint.Nodes.TokenNode && child.getFirstToken().getStr().toUpperCase() === "FROM") {
        break;
      }
      if (!(child instanceof abaplint.Nodes.ExpressionNode)) {
        return undefined;
      }
      const sep = children[i + 1];
      const word = sep instanceof abaplint.Nodes.TokenNode ? sep.getFirstToken().getStr().toUpperCase() : "";
      if ((word === "=" || word === "TO" || word === "INTO") && children[i + 2] instanceof abaplint.Nodes.ExpressionNode) {
        const name = child.concatTokens().toUpperCase();
        ret.push(`${JSON.stringify(name)}: ${traversal.traverse(children[i + 2]).getCode()}`);
        i = i + 3;
        continue;
      }
      if (!(child.get() instanceof abaplint.Expressions.Target)) {
        return undefined; // a dynamic parameter list (ptab) is not transpiled
      }
      // the short form: the parameter is the data object of that name
      ret.push(`${JSON.stringify(child.concatTokens().toUpperCase())}: ${traversal.traverse(child).getCode()}`);
      i = i + 1;
    }
    return ret.length === 0 ? undefined : ret;
  }

}

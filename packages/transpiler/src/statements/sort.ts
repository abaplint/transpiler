import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ComponentChainTranspiler, FieldChainTranspiler} from "../expressions";

export class SortTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const concat = node.concatTokens().toUpperCase();
    const components = node.getChildren().filter(c => c instanceof abaplint.Nodes.ExpressionNode
      && (c.get() instanceof abaplint.Expressions.ComponentChain || this.isDynamicName(c, traversal))) as abaplint.Nodes.ExpressionNode[];
    const options: string[] = [];

    if (concat.includes(" BY ") === false) {
      const descending = node.findDirectTokenByText("DESCENDING") !== undefined;
      if (descending === true) {
        options.push("descending: true");
      }
    } else if (components.length > 0) {
      const by: string[] = [];
      for (const c of components) {
        const next = this.findNextText(c, node);
        const component = c.get() instanceof abaplint.Expressions.Dynamic
          ? this.dynamicName(c, traversal)
          : `"${ComponentChainTranspiler.concat(c, traversal)}"`;
        if (next === "DESCENDING") {
          by.push(`{component: ${component}, descending: true}`);
        } else {
          by.push(`{component: ${component}}`);
        }
      }
      options.push(`by: [${by.join(",")}]`);
    }

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();
    return new Chunk().append("abap.statements.sort(" + target + ",{" + options.join(",") + "});", node, traversal);
  }

  // "BY (name)" with the component name in a literal or a character-like field; a sort order table
  // (abap_sortorder_tab) is not handled here
  private isDynamicName(c: abaplint.Nodes.ExpressionNode, traversal: Traversal): boolean {
    if (!(c.get() instanceof abaplint.Expressions.Dynamic)) {
      return false;
    }
    if (c.findFirstExpression(abaplint.Expressions.ConstantString)) {
      return true;
    }
    const field = c.findFirstExpression(abaplint.Expressions.FieldChain);
    if (field === undefined) {
      return false;
    }
    const token = field.getFirstToken();
    const type = traversal.findCurrentScopeByToken(token)?.findVariable(token.getStr())?.getType();
    return !(type instanceof abaplint.BasicTypes.TableType);
  }

  private dynamicName(c: abaplint.Nodes.ExpressionNode, traversal: Traversal): string {
    const constant = c.findFirstExpression(abaplint.Expressions.ConstantString);
    if (constant) {
      const str = constant.getFirstToken().getStr();
      return JSON.stringify(str.substring(1, str.length - 1).toLowerCase().trimEnd());
    }
    const field = c.findFirstExpression(abaplint.Expressions.FieldChain)!;
    return new FieldChainTranspiler(true).transpile(field, traversal).getCode() + ".toLowerCase().trimEnd()";
  }

  private findNextText(c: abaplint.Nodes.ExpressionNode, parent: abaplint.Nodes.StatementNode): string {
    const children = parent.getChildren();
    for (let i = 0; i < children.length; i++) {
      const element = children[i];
      if (element !== c) {
        continue;
      }
      const next = children[i + 1];
      if (next) {
        return next.concatTokens().toUpperCase();
      }
    }
    return "";
  }

}
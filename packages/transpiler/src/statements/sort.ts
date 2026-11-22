import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ComponentChainTranspiler, FieldChainTranspiler} from "../expressions";

export class SortTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const options: string[] = [];

    // additions before BY apply to the whole SORT, after BY each component can have its own order and AS TEXT,
    // a component without its own order takes the order of the SORT
    let descending = false;
    let text = false;
    let afterBy = false;
    const by: {component: string, order?: string, text: boolean}[] = [];
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.ExpressionNode) {
        if (afterBy && (c.get() instanceof abaplint.Expressions.ComponentChain || this.isDynamicName(c, traversal))) {
          const component = c.get() instanceof abaplint.Expressions.Dynamic
            ? this.dynamicName(c, traversal)
            : `"${ComponentChainTranspiler.concat(c, traversal)}"`;
          by.push({component, text: false});
        }
        continue;
      }
      const word = c.concatTokens().toUpperCase();
      const current = by[by.length - 1];
      if (word === "BY") {
        afterBy = true;
      } else if (word === "DESCENDING" || word === "ASCENDING") {
        if (current !== undefined) {
          current.order = word;
        } else {
          descending = word === "DESCENDING";
        }
      } else if (word === "TEXT") {
        if (current !== undefined) {
          current.text = true;
        } else {
          text = true;
        }
      }
    }

    if (by.length > 0) {
      const list = by.map(b => {
        const opts = [`component: ${b.component}`];
        if (b.order === "DESCENDING" || (b.order === undefined && descending)) {
          opts.push("descending: true");
        }
        if (b.text || text) {
          opts.push("text: true");
        }
        return "{" + opts.join(", ") + "}";
      });
      options.push(`by: [${list.join(",")}]`);
    } else if (afterBy === false) {
      if (descending === true) {
        options.push("descending: true");
      }
      if (text === true) {
        options.push("text: true");
      }
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

}
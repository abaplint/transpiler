import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";
import {UniqueIdentifier} from "../unique_identifier";
import {ComponentChainSimpleTranspiler} from "./component_chain_simple";

export class FilterBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("FilterBodyTranspiler, Expected TypeNameOrInfer");
    }

    const sources = body.findDirectExpressions(Expressions.Source);
    if (sources.length === 0) {
      throw new Error("FilterBodyTranspiler, source not found");
    }
    const source = traversal.traverse(sources[0]).getCode();
    const type = new TypeNameOrInfer().findType(typ, traversal);
    const target = TranspileTypes.toType(type);
    const whereNode = body.findDirectExpression(Expressions.ComponentCond);
    if (whereNode === undefined) {
      throw new Error("FilterBodyTranspiler, WHERE not found");
    }

    if (sources.length > 1) {
      return this.transpileIn(target, source, traversal.traverse(sources[1]).getCode(), whereNode,
        body.findDirectTokenByText("EXCEPT") !== undefined, body, traversal);
    }
    return this.transpileSingle(target, source, whereNode,
      body.findDirectTokenByText("EXCEPT") !== undefined, body, traversal);
  }

  private transpileSingle(target: string, source: string, whereNode: Nodes.ExpressionNode,
                          except: boolean, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const result = UniqueIdentifier.get();
    const row = UniqueIdentifier.get();
    const where = traversal.traverse(whereNode).getCode();
    const options: string[] = [];
    options.push(except ? `where: async (I) => !((${where})(I))` : `where: async ${where}`);
    const key = body.findDirectExpression(Expressions.SimpleName);
    if (key) {
      options.push(`usingKey: "${key.concatTokens().toLowerCase()}"`);
    }

    const ret = new Chunk();
    ret.appendString("(await (async () => {\n");
    ret.appendString(`const ${result} = ${target};\n`);
    ret.appendString(`for await (const ${row} of abap.statements.loop(${source}, {${options.join(", ")}})) {\n`);
    ret.appendString(`abap.statements.insertInternal({"table": ${result}, "data": ${row}});\n`);
    ret.appendString("}\n");
    ret.appendString(`return ${result};\n`);
    ret.appendString("})())");
    return ret;
  }

  private transpileIn(target: string, source: string, filterSource: string, whereNode: Nodes.ExpressionNode,
                      except: boolean, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const result = UniqueIdentifier.get();
    const sourceRow = UniqueIdentifier.get();
    const filterRow = UniqueIdentifier.get();
    const matched = UniqueIdentifier.get();
    const condition = this.transpileInCondition(whereNode, traversal, sourceRow, filterRow);
    const key = body.findDirectExpression(Expressions.SimpleName);
    const filterOptions = key ? `, {usingKey: "${key.concatTokens().toLowerCase()}"}` : "";
    const selection = except ? `!${matched}` : matched;

    const ret = new Chunk();
    ret.appendString("(await (async () => {\n");
    ret.appendString(`const ${result} = ${target};\n`);
    ret.appendString(`for await (const ${sourceRow} of abap.statements.loop(${source})) {\n`);
    ret.appendString(`let ${matched} = false;\n`);
    ret.appendString(`for await (const ${filterRow} of abap.statements.loop(${filterSource}${filterOptions})) {\n`);
    ret.appendString(`if (${condition}) {\n`);
    ret.appendString(`${matched} = true;\n`);
    ret.appendString("break;\n}\n}\n");
    ret.appendString(`if (${selection}) {\n`);
    ret.appendString(`abap.statements.insertInternal({"table": ${result}, "data": ${sourceRow}});\n`);
    ret.appendString("}\n}\n");
    ret.appendString(`return ${result};\n`);
    ret.appendString("})())");
    return ret;
  }

  private transpileInCondition(node: Nodes.ExpressionNode, traversal: Traversal,
                               sourceRow: string, filterRow: string): string {
    if (node.get() instanceof Expressions.ComponentCompare) {
      return this.transpileInCompare(node, traversal, sourceRow, filterRow);
    }

    let ret = "";
    for (const child of node.getChildren()) {
      if (child instanceof Nodes.ExpressionNode) {
        ret += this.transpileInCondition(child, traversal, sourceRow, filterRow);
      } else {
        switch (child.concatTokens().toUpperCase()) {
          case "AND": ret += " && "; break;
          case "OR": ret += " || "; break;
          case "NOT": ret += "!"; break;
          case "(": ret += "("; break;
          case ")": ret += ")"; break;
          default: throw new Error("FilterBodyTranspiler, unexpected condition token " + child.concatTokens());
        }
      }
    }
    return ret;
  }

  private transpileInCompare(node: Nodes.ExpressionNode, traversal: Traversal,
                             sourceRow: string, filterRow: string): string {
    const leftNode = node.findDirectExpression(Expressions.ComponentChainSimple);
    if (leftNode === undefined) {
      throw new Error("FilterBodyTranspiler, comparison component not found");
    }
    const left = new ComponentChainSimpleTranspiler(`${sourceRow}.get().`).transpile(leftNode, traversal).getCode();
    const sources = node.findDirectExpressions(Expressions.Source);
    const concat = node.concatTokens().toUpperCase();
    const negate = concat.startsWith("NOT ") ? "!" : "";

    const operator = node.findDirectExpression(Expressions.CompareOperator);
    if (operator && sources[0]) {
      const compare = traversal.traverse(operator).getCode();
      return `${negate}abap.compare.${compare}(${left}, ${this.filterOperand(sources[0], traversal, filterRow)})`;
    }
    if (concat.includes(" BETWEEN ") && sources.length === 2) {
      const between = `abap.compare.ge(${left}, ${this.filterOperand(sources[0], traversal, filterRow)}) && `
        + `abap.compare.le(${left}, ${this.filterOperand(sources[1], traversal, filterRow)})`;
      return concat.includes(" NOT BETWEEN ") ? `!(${between})` : `(${between})`;
    }
    if (concat.endsWith("IS INITIAL")) {
      return `${negate}abap.compare.initial(${left})`;
    } else if (concat.endsWith("IS NOT INITIAL")) {
      return `!abap.compare.initial(${left})`;
    }
    throw new Error("FilterBodyTranspiler, unsupported IN comparison " + node.concatTokens());
  }

  private filterOperand(source: Nodes.ExpressionNode, traversal: Traversal, filterRow: string): string {
    const code = traversal.traverse(source).getCode();
    const sourceField = source.findFirstExpression(Expressions.SourceField);
    const name = sourceField?.findDirectExpression(Expressions.Field)?.concatTokens();
    if (name === undefined) {
      return code;
    }
    const escaped = Traversal.escapeNamespace(name)?.replace("~", "$").toLowerCase();
    const variable = Traversal.prefixVariable(Traversal.escapeNamespace(name)!);
    if (escaped === undefined || code.startsWith(variable) === false) {
      return code;
    }
    return `${filterRow}.get().${escaped}` + code.substring(variable.length);
  }
}

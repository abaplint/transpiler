import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {TargetTranspiler} from "./target";
import {LetTranspiler} from "./let";
import {FieldSymbolTranspiler} from "../statements";
import {SourceFieldSymbolTranspiler} from "./source_field_symbol";
import {UniqueIdentifier} from "../unique_identifier";

interface LoopDescriptor {
  beforeLoop: string[];
  open: string;
  preBody: string[];
  postBody: string[];
  close: string;
}

export class ReduceBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("ReduceBodyTranspiler, Expected TypeNameOrInfer");
    }

    const forExpressions = body.findDirectExpressions(Expressions.For);
    if (forExpressions.length === 0) {
      throw new Error("ReduceBodyTranspiler, expected FOR");
    }

    const ret = new Chunk();
    ret.appendString("(await (async () => {\n");

    const outerLet = body.findDirectExpression(Expressions.Let);
    if (outerLet) {
      ret.appendString(new LetTranspiler().transpile(outerLet, traversal).getCode() + "\n");
    }

    const returnField = this.declareInit(body, traversal, ret);
    const declarations: string[] = [];
    const descriptors = forExpressions.map(forExpression =>
      this.describeFor(forExpression, body, traversal, declarations));
    for (const declaration of declarations) {
      ret.appendString(declaration + "\n");
    }

    let indent = "";
    const levelIndents: string[] = [];
    for (const descriptor of descriptors) {
      this.appendBlocks(ret, descriptor.beforeLoop, indent);
      ret.appendString(indent + descriptor.open + "\n");
      indent += "  ";
      levelIndents.push(indent);
      this.appendBlocks(ret, descriptor.preBody, indent);
    }

    this.appendBlock(ret, this.transpileNext(body, traversal), indent);

    for (let i = descriptors.length - 1; i >= 0; i--) {
      const descriptor = descriptors[i];
      const currentIndent = levelIndents[i];
      this.appendBlocks(ret, descriptor.postBody, currentIndent);
      indent = currentIndent.substring(0, Math.max(0, currentIndent.length - 2));
      ret.appendString(indent + descriptor.close + "\n");
    }

    ret.appendString(`return ${returnField};\n`);
    ret.appendString("})())");
    return ret;
  }

  private describeFor(forExpression: Nodes.ExpressionNode, body: Nodes.ExpressionNode,
                      traversal: Traversal, declarations: string[]): LoopDescriptor {
    if (forExpression.findDirectTokenByText("GROUPS")) {
      return this.describeGroupsFor(forExpression, body, traversal);
    }
    const loopExpression = forExpression.findDirectExpression(Expressions.InlineLoopDefinition);
    if (loopExpression === undefined) {
      return this.describeIndexFor(forExpression, body, traversal);
    }

    const sourceNode = loopExpression.findDirectExpression(Expressions.Source);
    if (sourceNode === undefined) {
      throw new Error("ReduceBodyTranspiler FOR missing source, " + body.concatTokens());
    }
    const loopSource = traversal.traverse(sourceNode).getCode();
    const options: string[] = [];

    const whereNode = forExpression.findDirectExpression(Expressions.ComponentCond);
    if (whereNode) {
      options.push("where: async " + traversal.traverse(whereNode).getCode());
    }
    const fromNode = forExpression.findExpressionAfterToken("FROM");
    if (fromNode && fromNode instanceof Nodes.ExpressionNode) {
      options.push("from: " + traversal.traverse(fromNode).getCode());
    }
    const toNode = forExpression.findExpressionAfterToken("TO");
    if (toNode && toNode instanceof Nodes.ExpressionNode) {
      options.push("to: " + traversal.traverse(toNode).getCode());
    }
    const keyNode = loopExpression.findExpressionAfterToken("KEY");
    if (keyNode) {
      options.push(`usingKey: "${keyNode.concatTokens().toLowerCase()}"`);
    }

    const unique = UniqueIdentifier.get();
    const preBody: string[] = [];
    const postBody: string[] = [];
    const fieldSymbol = loopExpression.findDirectExpression(Expressions.TargetFieldSymbol);
    if (fieldSymbol) {
      declarations.push(new FieldSymbolTranspiler().transpile(fieldSymbol, traversal).getCode());
      const target = new SourceFieldSymbolTranspiler().transpile(fieldSymbol, traversal).getCode();
      preBody.push(`${target}.assign(${unique});`);
      postBody.push(`${target}.unassign();`);
    } else {
      const field = loopExpression.findDirectExpression(Expressions.TargetField);
      if (field === undefined) {
        throw new Error("ReduceBodyTranspiler FOR missing target, " + body.concatTokens());
      }
      preBody.push(`const ${traversal.traverse(field).getCode()} = ${unique}.clone();`);
    }

    const indexTarget = loopExpression.findExpressionAfterToken("INTO");
    const beforeLoop: string[] = [];
    if (indexTarget && indexTarget instanceof Nodes.ExpressionNode) {
      const indexName = UniqueIdentifier.get();
      const indexCode = traversal.traverse(indexTarget).getCode();
      beforeLoop.push(`let ${indexName} = 1;`);
      preBody.push(`const ${indexCode} = new abap.types.Integer().set(${indexName});`);
      postBody.push(`${indexName}++;`);
    }

    const letNode = forExpression.findDirectExpression(Expressions.Let);
    if (letNode) {
      preBody.push(new LetTranspiler().transpile(letNode, traversal).getCode());
    }

    const opts = options.length === 0 ? "" : `, {${options.join(", ")}}`;
    return {
      beforeLoop,
      open: `for await (const ${unique} of abap.statements.loop(${loopSource}${opts})) {`,
      preBody,
      postBody,
      close: "}",
    };
  }

  private describeGroupsFor(forExpression: Nodes.ExpressionNode, body: Nodes.ExpressionNode,
                            traversal: Traversal): LoopDescriptor {
    const targets = forExpression.findDirectExpressions(Expressions.TargetField);
    const source = forExpression.findDirectExpression(Expressions.Source);
    const groupBy = forExpression.findDirectExpression(Expressions.FieldChain);
    if (targets.length !== 2 || source === undefined || groupBy === undefined) {
      throw new Error("ReduceBodyTranspiler invalid GROUPS FOR, " + body.concatTokens());
    }

    const groupTarget = traversal.traverse(targets[0]).getCode();
    const memberTarget = traversal.traverse(targets[1]).getCode();
    const sourceCode = traversal.traverse(source).getCode();
    const groupByCode = traversal.traverse(groupBy).getCode();
    const groups = UniqueIdentifier.get();
    const row = UniqueIdentifier.get();
    const key = UniqueIdentifier.get();
    const rawKey = UniqueIdentifier.get();
    const entry = UniqueIdentifier.get();

    const generator = `(async function*() {\n`
      + `const ${groups} = new Map();\n`
      + `for await (const ${row} of abap.statements.loop(${sourceCode})) {\n`
      + `const ${memberTarget} = ${row}.clone();\n`
      + `const ${key} = ${groupByCode};\n`
      + `const ${rawKey} = ${key}.get();\n`
      + `let ${entry} = ${groups}.get(${rawKey});\n`
      + `if (${entry} === undefined) {\n`
      + `${entry} = {key: ${key}.clone(), members: []};\n`
      + `${groups}.set(${rawKey}, ${entry});\n`
      + `}\n`
      + `${entry}.members.push(${row});\n`
      + `}\n`
      + `for (const value of ${groups}.values()) { yield value; }\n`
      + `})()`;

    const loopEntry = UniqueIdentifier.get();
    return {
      beforeLoop: [],
      open: `for await (const ${loopEntry} of ${generator}) {`,
      preBody: [
        `const ${groupTarget} = ${loopEntry}.key.clone();`,
        `const ${memberTarget} = ${loopEntry}.members[0].clone();`,
      ],
      postBody: [],
      close: "}",
    };
  }

  private describeIndexFor(forExpression: Nodes.ExpressionNode, body: Nodes.ExpressionNode,
                           traversal: Traversal): LoopDescriptor {
    const counter = forExpression.findDirectExpression(Expressions.InlineFieldDefinition);
    const cond = forExpression.findDirectExpression(Expressions.Cond);
    if (counter === undefined || cond === undefined) {
      throw new Error("ReduceBodyTranspiler invalid index FOR, " + body.concatTokens());
    }

    const hasUntil = forExpression.findDirectTokenByText("UNTIL") !== undefined;
    const hasWhile = forExpression.findDirectTokenByText("WHILE") !== undefined;
    if ((hasUntil ? 1 : 0) + (hasWhile ? 1 : 0) !== 1) {
      throw new Error("ReduceBodyTranspiler index FOR requires WHILE or UNTIL, " + body.concatTokens());
    }

    const fieldName = counter.findDirectExpression(Expressions.Field)?.concatTokens().toLowerCase();
    const source = counter.findDirectExpression(Expressions.Source);
    if (fieldName === undefined || source === undefined) {
      throw new Error("ReduceBodyTranspiler invalid index definition, " + body.concatTokens());
    }
    const variable = traversal.findCurrentScopeByToken(counter.getFirstToken())?.findVariable(fieldName);
    if (variable === undefined) {
      throw new Error(`ReduceBodyTranspiler: variable ${fieldName} not found`);
    }

    const counterName = Traversal.prefixVariable(fieldName);
    const thenExpr = forExpression.findExpressionAfterToken("THEN");
    const increment = thenExpr && thenExpr instanceof Nodes.ExpressionNode
      ? traversal.traverse(thenExpr).getCode()
      : `abap.operators.add(${counterName}, new abap.types.Integer().set(1))`;
    const condCode = traversal.traverse(cond).getCode();
    const preBody: string[] = [];
    const postBody = [`${counterName}.set(${increment});`];
    if (hasWhile) {
      preBody.push(`if (!(${condCode})) {\nbreak;\n}`);
    }
    const letNode = forExpression.findDirectExpression(Expressions.Let);
    if (letNode) {
      preBody.push(new LetTranspiler().transpile(letNode, traversal).getCode());
    }
    if (hasUntil) {
      postBody.push(`if (${condCode}) {\nbreak;\n}`);
    }

    return {
      beforeLoop: [TranspileTypes.declare(variable), `${counterName}.set(${traversal.traverse(source).getCode()});`],
      open: "while (true) {",
      preBody,
      postBody,
      close: "}",
    };
  }

  private declareInit(body: Nodes.ExpressionNode, traversal: Traversal, ret: Chunk): string {
    let returnField = "";
    for (const init of body.findDirectExpressions(Expressions.InlineFieldDefinition)) {
      const fieldName = init.findDirectExpression(Expressions.Field)?.concatTokens().toLowerCase();
      if (fieldName === undefined) {
        throw new Error("ReduceBodyTranspiler INIT missing field");
      }
      if (returnField === "") {
        returnField = Traversal.prefixVariable(fieldName);
      }
      const variable = traversal.findCurrentScopeByToken(init.getFirstToken())?.findVariable(fieldName);
      if (variable === undefined) {
        throw new Error(`ReduceBodyTranspiler: variable ${fieldName} not found`);
      }
      const target = Traversal.prefixVariable(fieldName);
      ret.appendString(TranspileTypes.declare(variable) + "\n");
      const source = init.findDirectExpression(Expressions.Source);
      if (source) {
        ret.appendString(`${target}.set(${traversal.traverse(source).getCode()});\n`);
      }
    }
    if (returnField === "") {
      throw new Error("ReduceBodyTranspiler INIT missing");
    }
    return returnField;
  }

  private transpileNext(body: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";
    const children = body.findDirectExpression(Expressions.ReduceNext)?.getChildren() || [];
    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      if (!(child instanceof Nodes.ExpressionNode) || !(child.get() instanceof Expressions.SimpleTarget)) {
        continue;
      }
      const source = children.slice(i + 1).find(candidate =>
        candidate instanceof Nodes.ExpressionNode && candidate.get() instanceof Expressions.Source);
      if (!(source instanceof Nodes.ExpressionNode)) {
        throw new Error("ReduceBodyTranspiler NEXT missing source");
      }
      const target = new TargetTranspiler().transpile(child, traversal).getCode();
      const value = traversal.traverse(source).getCode();
      const between = children.slice(i + 1, children.indexOf(source)).map(candidate => candidate.concatTokens()).join("");
      const operators: {[key: string]: string} = {
        "+=": "add",
        "-=": "minus",
        "*=": "multiply",
        "/=": "divide",
        "&&=": "concat",
      };
      const operator = operators[between];
      ret += operator
        ? `${target}.set(abap.operators.${operator}(${target}, ${value}));\n`
        : `${target}.set(${value});\n`;
      i = children.indexOf(source);
    }
    return ret;
  }

  private appendBlocks(ret: Chunk, blocks: string[], indent: string): void {
    for (const block of blocks) {
      this.appendBlock(ret, block, indent);
    }
  }

  private appendBlock(ret: Chunk, block: string, indent: string): void {
    for (const line of block.split("\n")) {
      if (line.trim() !== "") {
        ret.appendString(indent + line.replace(/\r/g, "") + "\n");
      }
    }
  }
}

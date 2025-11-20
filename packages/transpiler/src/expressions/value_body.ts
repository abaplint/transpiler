import {Expressions, Nodes, BasicTypes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {ValueBodyLineTranspiler} from "./value_body_line";
import {FieldAssignmentTranspiler} from "./field_assignment";
import {FieldSymbolTranspiler} from "../statements";
import {SourceFieldSymbolTranspiler} from "./source_field_symbol";
import {TranspileTypes} from "../transpile_types";
import {LetTranspiler} from "./let";

export class ValueBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("ValueBodyTranspiler, Expected TypeNameOrInfer");
    }

    let ret = new Chunk().appendString(new TypeNameOrInfer().transpile(typ, traversal).getCode());
    const context = new TypeNameOrInfer().findType(typ, traversal);
    if (context instanceof BasicTypes.VoidType || context instanceof BasicTypes.UnknownType) {
      // compile option is runtime error, or it failed during the validation step
      return new Chunk(TranspileTypes.toType(context));
    }

    let post = "";
    let extraFields = "";
    const hasLines = body.findDirectExpression(Expressions.ValueBodyLine) !== undefined;

    const children = body.getChildren();
    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      if (child.get() instanceof Expressions.FieldAssignment && child instanceof Nodes.ExpressionNode) {
        const transpiled = new FieldAssignmentTranspiler().transpile(child, traversal, context).getCode();
        if (hasLines === false) {
          ret.appendString(transpiled);
        } else {
          extraFields += transpiled;
        }
      } else if (child.get() instanceof Expressions.ValueBase && child instanceof Nodes.ExpressionNode) {
        const source = traversal.traverse(child.findDirectExpression(Expressions.Source));
        ret = new Chunk().appendString(source.getCode() + ".clone()");
      } else if (child.get() instanceof Expressions.ValueBodyLine && child instanceof Nodes.ExpressionNode) {
        if (!(context instanceof BasicTypes.TableType)) {
          throw new Error("ValueBodyTranspiler, Expected BasicTypes, " + body.concatTokens());
        }
        const rowType = context.getRowType();
        ret.appendString(new ValueBodyLineTranspiler().transpile(rowType, child, traversal, extraFields).getCode());
      } else if (child.get() instanceof Expressions.Source && child instanceof Nodes.ExpressionNode) {
        const source = traversal.traverse(child);
        ret.appendString(".set(" + source.getCode() + ".clone())");
      } else if (child.get() instanceof Expressions.For && child instanceof Nodes.ExpressionNode) {
        const forNodes: Nodes.ExpressionNode[] = [];
        let idx = i;
        while (idx < children.length) {
          const candidate = children[idx];
          if (candidate.get() instanceof Expressions.For && candidate instanceof Nodes.ExpressionNode) {
            forNodes.push(candidate);
            idx++;
          } else {
            break;
          }
        }
        i = idx - 1;
        const result = this.buildForChain(forNodes, typ, traversal, body);
        ret = result.chunk;
        post = result.post;
      } else if (child instanceof Nodes.TokenNode && child.getFirstToken().getStr().toUpperCase() === "DEFAULT") {
        // note: this is last in the body, so its okay to prepend and postpend
        const sources = body.findDirectExpressions(Expressions.Source);
        const deflt = traversal.traverse(sources[1]).getCode();
        const pre = `(await (async () => { try { return `;
        ret = new Chunk().appendString(pre + ret.getCode());
        post += `; } catch (error) { if (abap.isLineNotFound(error)) { return ${deflt}; } throw error; } })())`;
      } else if (child instanceof Nodes.TokenNode && child.getFirstToken().getStr().toUpperCase() === "OPTIONAL") {
        // note: this is last in the body, so its okay to prepend and postpend
        const pre = `(await (async () => { try { return `;
        ret = new Chunk().appendString(pre + ret.getCode());
        post += `; } catch (error) { if (abap.isLineNotFound(error)) { return ${TranspileTypes.toType(context)}; } throw error; } })())`;
      } else {
        throw new Error("ValueBodyTranspiler, unknown " + child.get().constructor.name + " \"" + child.concatTokens()) + "\"";
      }
    }

    return ret.appendString(post);
  }

  private buildForChain(
      forNodes: Nodes.ExpressionNode[],
      typ: Nodes.ExpressionNode,
      traversal: Traversal,
      body: Nodes.ExpressionNode): {chunk: Chunk, post: string} {

    interface LoopDescriptor {
      beforeLoop: string[];
      open: string;
      preBody: string[];
      postBody: string[];
      close: string;
    }

    const val = new TypeNameOrInfer().transpile(typ, traversal).getCode();
    const chunk = new Chunk();
    const preLoopDecls: string[] = [];
    const descriptors: LoopDescriptor[] = [];
    const levelIndents: string[] = [];
    let uniqueCounter = 1;

    for (const child of forNodes) {
      const loop = child.findDirectExpression(Expressions.InlineLoopDefinition);
      if (loop) {
        if (["THEN", "UNTIL", "WHILE", "FROM", "TO", "GROUPS"].some(token => child.findDirectTokenByText(token))) {
          throw new Error("ValueBody FOR todo, " + body.concatTokens());
        }

        let loopWhere = "";
        const whereNode = child.findDirectExpression(Expressions.ComponentCond);
        if (whereNode) {
          const where = traversal.traverse(whereNode).getCode();
          loopWhere = `, {"where": async ` + where + `}`;
        }

        const base = loop.findDirectExpression(Expressions.ValueBase);
        if (base) {
          throw new Error("ValueBody FOR todo, base, " + body.concatTokens());
        }

        let targetDeclare = "";
        let targetAction = "";
        const fs = loop.findDirectExpression(Expressions.TargetFieldSymbol);
        const uniqueName = `unique${uniqueCounter++}`;
        if (fs) {
          targetDeclare = new FieldSymbolTranspiler().transpile(fs, traversal).getCode();
          const targetName = new SourceFieldSymbolTranspiler().transpile(fs, traversal).getCode();
          targetAction = `${targetName}.assign(${uniqueName});`;
        } else {
          const field = traversal.traverse(loop.findDirectExpression(Expressions.TargetField));
          if (field === undefined) {
            throw new Error("ValueBody FOR empty field todo, " + body.concatTokens());
          }
          targetAction = `const ${field.getCode()} = ${uniqueName}.clone();`;
        }

        const llet = child.findDirectExpression(Expressions.Let);
        if (llet) {
          targetAction += new LetTranspiler().transpile(llet, traversal).getCode();
        }

        const preBodyStatements: string[] = [];
        if (targetAction !== "") {
          preBodyStatements.push(targetAction);
        }

        const source = traversal.traverse(loop.findDirectExpression(Expressions.Source)).getCode();

        if (targetDeclare !== "") {
          preLoopDecls.push(targetDeclare);
        }

        const descriptor: LoopDescriptor = {
          beforeLoop: [],
          open: `for await (const ${uniqueName} of abap.statements.loop(${source}${loopWhere})) {`,
          preBody: preBodyStatements,
          postBody: [],
          close: "}",
        };

        const loopTokens = loop.concatTokens().toUpperCase();
        const indexTarget = loopTokens.includes("INDEX INTO") ? loop.findExpressionAfterToken("INTO") : undefined;
        if (indexTarget && indexTarget instanceof Nodes.ExpressionNode) {
          const indexCode = traversal.traverse(indexTarget).getCode();
          const counterName = `unique${uniqueCounter++}`;
          descriptor.beforeLoop.push(`let ${counterName} = 1;`);
          descriptor.preBody.push(`const ${indexCode} = new abap.types.Integer().set(${counterName});`);
          descriptor.postBody.push(`${counterName}++;`);
        }

        descriptors.push(descriptor);
      } else {
        const counter = child.findDirectExpression(Expressions.InlineFieldDefinition);
        if (counter === undefined) {
          throw new Error("ValueBody FOR todo, " + body.concatTokens());
        }

        if (["GROUPS", "FROM", "TO"].some(token => child.findDirectTokenByText(token))) {
          throw new Error("ValueBody FOR todo, " + body.concatTokens());
        }

        if (child.findDirectExpression(Expressions.ComponentCond)) {
          throw new Error("ValueBody FOR todo, component cond, " + body.concatTokens());
        }

        const cond = child.findDirectExpression(Expressions.Cond);
        if (cond === undefined) {
          throw new Error("ValueBody FOR missing condition, " + body.concatTokens());
        }

        const hasUntil = child.findDirectTokenByText("UNTIL") !== undefined;
        const hasWhile = child.findDirectTokenByText("WHILE") !== undefined;
        if ((hasUntil ? 1 : 0) + (hasWhile ? 1 : 0) !== 1) {
          throw new Error("ValueBody FOR todo, condition, " + body.concatTokens());
        }

        const fieldExpr = counter.findDirectExpression(Expressions.Field);
        const fieldName = fieldExpr?.concatTokens().toLowerCase();
        if (fieldName === undefined) {
          throw new Error("ValueBody FOR todo, inline field, " + body.concatTokens());
        }
        const scope = traversal.findCurrentScopeByToken(counter.getFirstToken());
        const variable = scope?.findVariable(fieldName);
        if (variable === undefined) {
          throw new Error("ValueBody FOR todo, variable, " + body.concatTokens());
        }
        const declare = TranspileTypes.declare(variable);
        const counterName = Traversal.prefixVariable(fieldName);

        const startSource = counter.findDirectExpression(Expressions.Source);
        if (startSource === undefined) {
          throw new Error("ValueBody FOR missing initial value, " + body.concatTokens());
        }
        const start = traversal.traverse(startSource).getCode();

        const thenExpr = child.findExpressionAfterToken("THEN");
        let incrementExpression = "";
        if (thenExpr && thenExpr instanceof Nodes.ExpressionNode) {
          incrementExpression = traversal.traverse(thenExpr).getCode();
        } else {
          incrementExpression = `abap.operators.add(${counterName}, new abap.types.Integer().set(1))`;
        }
        const incrementLine = `${counterName}.set(${incrementExpression});`;

        const llet = child.findDirectExpression(Expressions.Let);
        let letCode = "";
        if (llet) {
          letCode = new LetTranspiler().transpile(llet, traversal).getCode();
        }

        const condCode = traversal.traverse(cond).getCode();

        const preCheck = hasWhile ? `if (!(${condCode})) {\n  break;\n}` : "";
        const postLoop: string[] = [];
        postLoop.push(incrementLine);
        if (hasUntil) {
          postLoop.push(`if (${condCode}) {\n  break;\n}`);
        }

        descriptors.push({
          beforeLoop: [declare, `${counterName}.set(${start});`],
          open: "while (true) {",
          preBody: [preCheck, letCode].filter(s => s !== ""),
          postBody: postLoop,
          close: "}",
        });
      }
    }

    if (descriptors.length === 0) {
      throw new Error("ValueBodyTranspiler FOR internal error");
    }

    chunk.appendString("await (async () => {\n");
    this.appendBlocks(chunk, preLoopDecls, "");
    chunk.appendString(`const VAL = ${val};\n`);

    let indent = "";
    for (const desc of descriptors) {
      this.appendBlocks(chunk, desc.beforeLoop, indent);
      chunk.appendString(indent + desc.open + "\n");
      indent += "  ";
      levelIndents.push(indent);
      this.appendBlocks(chunk, desc.preBody, indent);
    }

    chunk.appendString(indent + "VAL");

    let post = ";\n";
    for (let i = descriptors.length - 1; i >= 0; i--) {
      const desc = descriptors[i];
      const currentIndent = levelIndents[i] ?? "";
      post += this.blocksToString(desc.postBody, currentIndent);
      const parentIndent = currentIndent.substring(0, Math.max(0, currentIndent.length - 2));
      post += parentIndent + desc.close + "\n";
    }
    post += "return VAL;\n})()";

    return {chunk, post};
  }

  private appendBlocks(chunk: Chunk, blocks: string[], indent: string): void {
    for (const block of blocks) {
      this.appendBlock(chunk, block, indent);
    }
  }

  private appendBlock(chunk: Chunk, block: string, indent: string): void {
    if (block === "") {
      return;
    }
    const lines = block.split("\n");
    for (const line of lines) {
      const clean = line.replace(/\r/g, "");
      if (clean.trim() === "") {
        continue;
      }
      chunk.appendString(indent + clean + "\n");
    }
  }

  private blocksToString(blocks: string[], indent: string): string {
    let ret = "";
    for (const block of blocks) {
      if (block === "") {
        continue;
      }
      const lines = block.split("\n");
      for (const line of lines) {
        const clean = line.replace(/\r/g, "");
        if (clean.trim() === "") {
          continue;
        }
        ret += indent + clean + "\n";
      }
    }
    return ret;
  }

}
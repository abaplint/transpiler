import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TranspileTypes} from "../transpile_types";
import {Traversal} from "../traversal";
import {ConstantTranspiler} from "../expressions/constant";
import {FieldChainTranspiler} from "../expressions";
import {Chunk} from "../chunk";

export class DataTranspiler implements IStatementTranspiler {
  private readonly skipLoopScoping: boolean;
  private variableName = "";
  private loopScoped = false;

  public constructor(options?: {skipLoopScoping?: boolean}) {
    this.skipLoopScoping = options?.skipLoopScoping === true;
  }

  /** name of the declared javascript variable, set by transpile() */
  public getVariableName(): string {
    return this.variableName;
  }

  /** set by transpile(), true if the declaration is inside a loop */
  public isLoopScoped(): boolean {
    return this.loopScoped;
  }

  /** DATA is scoped to the enclosing method/form/program, but when the statement is inside a
   * loop the generated declaration ends up inside the javascript block of the loop. Declaring
   * with "var" hoists it out of the block, and the guard makes sure its constructed only on the
   * first pass, ie. contents are kept across iterations, matching ABAP */
  public static wrapLoopScoped(name: string, chunk: Chunk): Chunk {
    return new Chunk()
      .appendString(`if (${name} === undefined) {\n`)
      .appendChunk(chunk)
      .appendString("\n}");
  }

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const token = node.findFirstExpression(abaplint.Expressions.DefinitionName)?.getFirstToken();
    if (token === undefined) {
      throw new Error("DataTranspiler, token not found");
    }

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("DataTranspiler, scope not found: " + node.concatTokens());
    }

    const found = scope.findVariable(token.getStr());
    if (found === undefined) {
      throw new Error("DataTranspiler, var not found, \"" + token.getStr() + "\", " + traversal.getFilename() + ", line: " + token.getRow());
    }

    let value = "";
    if (found.getValue() !== undefined && node.concatTokens().includes(" & ")) {
      value = "\n" + traversal.setValues(found, found.getName());
    } else {
      value = DataTranspiler.buildValue(node, Traversal.prefixVariable(found.getName().toLowerCase()), traversal);
    }

    // for enum types, initialize with the first enum value
    if (found.getType() instanceof abaplint.BasicTypes.EnumType && value === "") {
      const enumDefault = DataTranspiler.findEnumDefault(scope, found.getType());
      if (enumDefault) {
        value = "\n" + Traversal.prefixVariable(found.getName().toLowerCase()) + ".set(\"" + enumDefault + "\");";
      }
    }

    this.variableName = Traversal.prefixVariable(Traversal.escapeNamespace(found.getName().toLowerCase()));
    this.loopScoped = traversal.isInsideLoop(node);

    const ret = new Chunk()
      .appendString(this.loopScoped === true ? "var " : "let ")
      .appendString(this.variableName)
      .appendString(" = " + TranspileTypes.toType(found.getType()))
      .appendString(";")
      .appendString(value);

    if (this.loopScoped === true && this.skipLoopScoping === false) {
      return DataTranspiler.wrapLoopScoped(this.variableName, ret);
    }

    return ret;
  }

  public static findEnumDefault(scope: abaplint.ISpaghettiScopeNode, _enumType: abaplint.AbstractType): string | undefined {
    let current: abaplint.ISpaghettiScopeNode | undefined = scope;
    while (current) {
      const vars = current.getData().vars;
      for (const key of Object.keys(vars)) {
        const v = vars[key];
        if (v.getMeta().includes(abaplint.IdentifierMeta.Enum)) {
          const structType = v.getType();
          if (structType instanceof abaplint.BasicTypes.StructureType) {
            const components = structType.getComponents();
            if (components.length > 0) {
              return components[0].name.toUpperCase();
            }
          }
        }
      }
      current = current.getParent();
    }
    return undefined;
  }

  public static buildValue(node: abaplint.Nodes.StatementNode, name: string, traversal: Traversal): string {
    let value = "";
    const val = node.findFirstExpression(abaplint.Expressions.Value);
    if (val) {
      let int = val.findFirstExpression(abaplint.Expressions.Integer);
      if (int === undefined) {
        int = val.findFirstExpression(abaplint.Expressions.ConstantString);
      }
      if (int) {
        const escaped = ConstantTranspiler.escape(ConstantTranspiler.trimTextFieldLiteral(int.concatTokens()));
        value = "\n" + name + ".set(" + escaped + ");";
      } else if (val.getChildren()[1].get() instanceof abaplint.Expressions.SimpleFieldChain) {
        const s = new FieldChainTranspiler().transpile(val.getChildren()[1] as abaplint.Nodes.ExpressionNode, traversal).getCode();
        value = "\n" + name + ".set(" + s + ");";
      }
    }
    return value;
  }

}
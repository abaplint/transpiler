import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

/** The kernel class the EML statements are lowered to. One call per
 * operation of a statement; the names travel as strings, the FROM / WITH
 * table and the MAPPED, FAILED, REPORTED and RESULT targets as they are,
 * and only the targets the statement names are passed, the kernel binds
 * the rest. */
export const KERNEL_RAP_EML = "KERNEL_RAP_EML";

export function abapString(value: string): string {
  return `new abap.types.String().set(${JSON.stringify(value)})`;
}

export function abapFlag(): string {
  return `abap.CharacterFactory.get(1, 'X')`;
}

/** the EML forms this lowering does not handle, named in the statement */
export function unsupportedAddition(node: abaplint.Nodes.StatementNode, additions: string[]): string | undefined {
  const tokens = node.getChildren().filter(c => c instanceof abaplint.Nodes.TokenNode).map(c => c.getFirstToken().getStr().toUpperCase());
  const text = " " + tokens.join(" ") + " ";
  for (const addition of additions) {
    if (text.includes(" " + addition + " ")) {
      return addition;
    }
  }
  if (node.findAllExpressions(abaplint.Expressions.EMLEntityPath).length > 0
      || node.findAllExpressions(abaplint.Expressions.EntityAssociation).length > 0) {
    return "BY";
  }
  return undefined;
}

export function emitCalls(name: string, lookup: string, calls: string[], node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
  const ret = `if (${lookup} === undefined) throw new Error("${name}, kernel class missing");\n` + calls.join("\n");
  return new Chunk().append(ret, node, traversal);
}

/** a walker over the flat token and expression sequence of an EML statement */
export class EMLWalker {
  private readonly children: readonly (abaplint.Nodes.TokenNode | abaplint.Nodes.ExpressionNode)[];
  private index = 0;

  public constructor(node: abaplint.Nodes.StatementNode) {
    this.children = node.getChildren();
  }

  public done(): boolean {
    return this.index >= this.children.length;
  }

  /** the next child as an upper case keyword, or undefined for an expression */
  public peekKeyword(): string | undefined {
    const child = this.children[this.index];
    if (child instanceof abaplint.Nodes.TokenNode) {
      return child.getFirstToken().getStr().toUpperCase();
    }
    return undefined;
  }

  public takeKeyword(): string | undefined {
    const keyword = this.peekKeyword();
    if (keyword !== undefined) {
      this.index++;
    }
    return keyword;
  }

  /** consume the keywords in order, or nothing when they are not next */
  public accept(...keywords: string[]): boolean {
    for (let i = 0; i < keywords.length; i++) {
      const child = this.children[this.index + i];
      if (!(child instanceof abaplint.Nodes.TokenNode) || child.getFirstToken().getStr().toUpperCase() !== keywords[i]) {
        return false;
      }
    }
    this.index += keywords.length;
    return true;
  }

  public takeExpression(): abaplint.Nodes.ExpressionNode {
    const child = this.children[this.index];
    if (!(child instanceof abaplint.Nodes.ExpressionNode)) {
      throw new Error("EML: expected an expression, got " + (child?.concatTokens() ?? "the end of the statement"));
    }
    this.index++;
    return child;
  }

  /** an entity, action or behavior definition name, upper case */
  public takeName(): string {
    return this.takeExpression().concatTokens().toUpperCase();
  }

  /** the SimpleNames of FIELDS ( a b ) up to the closing parenthesis, upper case */
  public takeFieldList(): string[] {
    const names: string[] = [];
    this.accept("(");
    while (!this.done() && this.peekKeyword() !== ")") {
      names.push(this.takeName());
    }
    this.accept(")");
    return names;
  }
}

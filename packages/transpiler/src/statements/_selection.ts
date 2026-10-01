import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";

/** PARAMETERS and SELECT-OPTIONS declare program variables that exist, with their
 * DEFAULT values, before INITIALIZATION runs. Without LOWER CASE a character-like
 * default is upper-cased (measured on a 7.5x system: 'abcdef' into a parameter of
 * length 4 is "ABCD", a string parameter and the LOW of a select-option likewise) */
export class SelectionDefault {

  public static variable(node: abaplint.Nodes.StatementNode,
                         traversal: Traversal): {name: string, type: abaplint.AbstractType} | undefined {
    const token = node.findDirectExpression(abaplint.Expressions.FieldSub)?.getFirstToken();
    if (token === undefined) {
      return undefined;
    }
    const found = traversal.findCurrentScopeByToken(token)?.findVariable(token.getStr());
    if (found === undefined) {
      return undefined;
    }
    return {
      name: Traversal.prefixVariable(Traversal.escapeNamespace(found.getName().toLowerCase())),
      type: found.getType(),
    };
  }

  /** the expression after a keyword of the statement, eg. DEFAULT; only a keyword
   * token counts, an operand named like the keyword does not */
  public static after(node: abaplint.Nodes.StatementNode, keyword: string): abaplint.Nodes.ExpressionNode | undefined {
    const children = node.getChildren();
    for (let i = 0; i < children.length - 1; i++) {
      if (children[i] instanceof abaplint.Nodes.TokenNode && children[i].concatTokens().toUpperCase() === keyword) {
        return children[i + 1] as abaplint.Nodes.ExpressionNode;
      }
    }
    return undefined;
  }

  /** sets the target to a DEFAULT operand, a Constant or a FieldChain, and upper-cases
   * it when the target is character-like and the statement has no LOWER CASE */
  public static set(node: abaplint.Nodes.StatementNode, target: string, targetType: abaplint.AbstractType,
                    operand: abaplint.Nodes.ExpressionNode, traversal: Traversal): string {
    let code = "\n" + target + ".set(" + traversal.traverse(operand).getCode() + ");";
    const characterLike = targetType instanceof abaplint.BasicTypes.CharacterType
      || targetType instanceof abaplint.BasicTypes.StringType;
    if (node.findDirectTokenByText("LOWER") === undefined && characterLike === true) {
      code += "\n" + target + ".set(abap.builtin.to_upper({val: " + target + "}));";
    }
    return code;
  }

  /** after the default: the value a SUBMIT ... WITH gave, which the runtime knows */
  public static submitted(node: abaplint.Nodes.StatementNode, traversal: Traversal, name: string, method: string): string {
    const obj = traversal.getCurrentObject();
    if (!(obj instanceof abaplint.Objects.Program)) {
      return "";
    }
    const selection = node.findDirectExpression(abaplint.Expressions.FieldSub)!.concatTokens().toUpperCase();
    const lower = node.findDirectTokenByText("LOWER") !== undefined;
    return "\nabap.statements." + method + "(" + JSON.stringify(obj.getName().toUpperCase()) + ", "
      + JSON.stringify(selection) + ", " + name + ", " + lower + ");";
  }

}

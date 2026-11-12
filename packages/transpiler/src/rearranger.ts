import {Expressions, INode, Nodes} from "@abaplint/core";

// this rearranges the AST to take precedence into account

export class Rearranger {

  public run(type: string, node: Nodes.StructureNode | undefined): Nodes.StructureNode | undefined {
    if (!node) {
      return undefined;
    }

    if (type === "INTF") {
// no arithmethic expressions in global interfaces
      return node;
    }

    const flattened = this.flatten(node);
    // the event blocks of a report are put in the order of the runtime by the traversal, see ReportEvents
    return this.rebuild(flattened) as Nodes.StructureNode;
  }

/////////////////

  private rebuild(node: INode): INode {
    if (node instanceof Nodes.TokenNode) {
      return node;
    }

    const children = node.getChildren();
    children.forEach(this.rebuild.bind(this));

    if (node instanceof Nodes.ExpressionNode) {
      this.precedence(node);
    }

    return node;
  }

  // this takes a single flattened node, and splits into binary nodes according to precedence and left to right processing
  private precedence(node: Nodes.ExpressionNode) {
    const children = node.getChildren();
    const arith = node.findDirectExpressions(Expressions.ArithOperator);
    // after flattening it might have multiple operators under the samenode
    if (arith.length <= 1) {
      return;
    }

    let splitAt: Nodes.ExpressionNode | undefined;

    // lowest precedence: +, - (skip **, *, /, MOD, DIV)
    for (let i = arith.length - 1; i >= 0; i--) {
      const a = arith[i];
      const concat = a.concatTokens().toUpperCase();
      if (concat === "*"
          || concat === "/"
          || concat === "**"
          || concat === "MOD"
          || concat === "DIV") {
        continue;
      }
      splitAt = a;
      break;
    }

    // medium precedence: *, /, MOD, DIV (skip only **)
    if (splitAt === undefined) {
      for (let i = arith.length - 1; i >= 0; i--) {
        const a = arith[i];
        const concat = a.concatTokens().toUpperCase();
        if (concat === "**") {
          continue;
        }
        splitAt = a;
        break;
      }
    }

    // fallback: all ** operators
    if (splitAt === undefined) {
      splitAt = arith[arith.length - 1];
    }

    const index = children.indexOf(splitAt);

    let left: (Nodes.TokenNode | Nodes.ExpressionNode)[] = [];
    {
      const lhs = children.slice(0, index);
      if (lhs.length > 1) {
        const temp = new Nodes.ExpressionNode(node.get());
        temp.setChildren(lhs);
        this.precedence(temp);
        left.push(temp);
      } else {
        left = lhs;
      }
    }

    let right: (Nodes.TokenNode | Nodes.ExpressionNode)[] = [];
    {
      const rhs = children.slice(index + 1);
      if (rhs.length > 1) {
        const temp = new Nodes.ExpressionNode(node.get());
        temp.setChildren(rhs);
        this.precedence(temp);
        right.push(temp);
      } else {
        right = rhs;
      }
    }

    node.setChildren(left.concat([splitAt]).concat(right));
  }

  // this flattens the arithmethic expressions so all related is under the same node
  private flatten(node: INode): INode {
    if (node instanceof Nodes.TokenNode) {
      return node;
    }

    const children = node.getChildren();
    children.forEach(this.flatten.bind(this));

    const last = children[children.length - 1];
    const secondLast = children[children.length - 2];
    if (last === undefined
        || secondLast === undefined
        || !(last instanceof Nodes.ExpressionNode)
        || !(last.get() instanceof Expressions.Source)
        || !(secondLast instanceof Nodes.ExpressionNode)
        || !(secondLast.get() instanceof Expressions.ArithOperator)) {
      return node;
    }

    const nestedArith = last.findDirectExpressions(Expressions.ArithOperator);
    if (nestedArith.length === 0) {
      return node;
    }

    const withoutLast = node.getChildren().slice(0, children.length - 1);

    // The nested children come up so that precedence can be decided across
    // the whole chain, and everything before the nested operator has to
    // arrive as one operand. For "a * b / c" that is already true, b being a
    // single Source. For "a * CONV f( b ) / c" it is not: CONV is five
    // children, and hoisting them one by one puts the type name and the
    // body beside the arithmetic, where the transpiler reads them as
    // operands and loses the rest of the expression. So the head is wrapped
    // back into one Source before it moves.
    const nested = last.getChildren();
    const firstOperator = nested.findIndex((c) =>
      c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ArithOperator);
    let head = nested.slice(0, firstOperator);
    if (head.length > 1) {
      const operand = new Nodes.ExpressionNode(last.get());
      operand.setChildren(head);
      head = [operand];
    }

    const flat = withoutLast.concat(head, nested.slice(firstOperator));
    node.setChildren(flat);

    return node;
  }

}
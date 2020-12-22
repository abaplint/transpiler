import {Expressions, INode, Nodes} from "@abaplint/core";

export class Rearranger {

  public run(node: Nodes.StructureNode | undefined): Nodes.StructureNode | undefined {
    if (!node) {
      return undefined;
    }

    const flattened = this.flatten(node);
    const rebuilt = this.rebuild(flattened);
    return rebuilt as Nodes.StructureNode;
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

    // todo: multiplication/division

    // left to right
    const lastArith = arith[arith.length - 1];
    const lastArithIndex = children.indexOf(lastArith);

    const lhs = children.slice(0, lastArithIndex);
    const left = new Nodes.ExpressionNode(node.get());
    left.setChildren(lhs);
    this.precedence(left);

    const rhs = children.slice(lastArithIndex + 1);

    node.setChildren([left as Nodes.TokenNode | Nodes.ExpressionNode, lastArith].concat(rhs));
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

    const withoutLast = node.getChildren().slice(0, children.length - 1);
    const flat = withoutLast.concat(last.getChildren());
    node.setChildren(flat);

    return node;
  }

}
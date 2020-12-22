import {Expressions, INode, Nodes} from "@abaplint/core";

export class Rearranger {
  public run(node: INode | undefined): INode | undefined {
    if (!node) {
      return;
    }

    return this.apply(node);
  }

  private apply(node: INode): INode {
    if (node instanceof Nodes.TokenNode) {
      return node;
    }

    node.setChildren(node.getChildren().map(c => {
      if (c.get() instanceof Expressions.Source) {
        return this.applySource(c as Nodes.ExpressionNode);
      } else {
        return this.apply(c);
      }
    }));

    return node;
  }

  private applySource(node: Nodes.ExpressionNode): INode {
    const prec = ["*", "/", "+", "-"];

    const lOp = node.findDirectExpression(Expressions.ArithOperator);
    const src = node.findDirectExpression(Expressions.Source);
    const rOp = src?.findDirectExpression(Expressions.ArithOperator);

    if (!lOp || !src || !rOp) {
      return this.apply(node);
    }

    const lPrec = prec.indexOf(lOp.getFirstToken().getStr());
    const rPrec = prec.indexOf(rOp.getFirstToken().getStr());

    if (lPrec >= rPrec) {
      return this.apply(node);
    }

    const l1 = node.getChildren();
    const l2 = src.getChildren();

    const n1 = l1.map(e => e === src ? l2[0] : e);
    const n2 = [node as Nodes.ExpressionNode | Nodes.TokenNode].concat(l2.slice(1));

    node.setChildren(n1);
    src.setChildren(n2);

    return src;
  }
}
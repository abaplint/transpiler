import {Expressions, INode, Nodes, Statements, Structures} from "@abaplint/core";

// this rearranges the AST to take precedence into account

enum EventKind {
  None,
  Start,
  End,
}

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
    const rebuilt = this.rebuild(flattened);
    const rearranged = this.rearrangeListEvents(rebuilt as Nodes.StructureNode);
    return rearranged;
  }

  /** Rearranges list event blocks so START-OF-SELECTION runs before END-OF-SELECTION,
   *  matching ABAP runtime behavior regardless of source order */
  private rearrangeListEvents(node: Nodes.StructureNode): Nodes.StructureNode {
    if (!(node.get() instanceof Structures.Any)) {
      return node;
    }

    const children = node.getChildren();

    // Check if there are any list event statements
    const hasStartOfSelection = children.some(c => c instanceof Nodes.StatementNode && c.get() instanceof Statements.StartOfSelection);
    const hasEndOfSelection = children.some(c => c instanceof Nodes.StatementNode && c.get() instanceof Statements.EndOfSelection);

    if (!hasStartOfSelection && !hasEndOfSelection) {
      return node;
    }

    // Split children into: preamble (before first event), event blocks (keyed by event type)
    const preamble: (Nodes.StatementNode | Nodes.StructureNode)[] = [];
    const startBlocks: (Nodes.StatementNode | Nodes.StructureNode)[] = [];
    const endBlocks: (Nodes.StatementNode | Nodes.StructureNode)[] = [];

    let currentEvent = EventKind.None;

    for (const child of children) {
      if (child instanceof Nodes.StatementNode && child.get() instanceof Statements.StartOfSelection) {
        currentEvent = EventKind.Start;
        startBlocks.push(child);
      } else if (child instanceof Nodes.StatementNode && child.get() instanceof Statements.EndOfSelection) {
        currentEvent = EventKind.End;
        endBlocks.push(child);
      } else {
        switch (currentEvent) {
          case EventKind.None:
            preamble.push(child as Nodes.StatementNode | Nodes.StructureNode);
            break;
          case EventKind.Start:
            startBlocks.push(child as Nodes.StatementNode | Nodes.StructureNode);
            break;
          case EventKind.End:
            endBlocks.push(child as Nodes.StatementNode | Nodes.StructureNode);
            break;
          default:
            break;
        }
      }
    }

    // Reassemble: preamble, then START-OF-SELECTION blocks, then END-OF-SELECTION blocks
    const newChildren = [...preamble, ...startBlocks, ...endBlocks];
    node.setChildren(newChildren);

    return node;
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

    // multiplication/division and left to right
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

    // fallback
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
    const flat = withoutLast.concat(last.getChildren());
    node.setChildren(flat);

    return node;
  }

}
import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class MethodSourceTranspiler implements IExpressionTranspiler {
  private readonly prepend: string;

  public constructor(prepend?: string) {
    this.prepend = (prepend || "") + "await ";
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const children = node.getChildren();
    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      const nextChild = children[i + 1];

      if (child.get() instanceof Expressions.ClassName) {
        ret.appendString(traversal.lookupClassOrInterface(child.concatTokens(), child.getFirstToken()));
      } else if (child.get() instanceof Expressions.Dynamic && nextChild?.concatTokens() === "=>") {
        const second = child.getChildren()[1];
        if (second.get() instanceof Expressions.FieldChain) {
          ret.appendChunk(traversal.traverse(second));
        } else if (second.get() instanceof Expressions.Constant) {
          const lookup = traversal.lookupClassOrInterface(second.getFirstToken().getStr(), child.getFirstToken(), true);
          const lookupException = traversal.lookupClassOrInterface("'CX_SY_DYN_CALL_ILLEGAL_CLASS'", child.getFirstToken(), true);
          // eslint-disable-next-line max-len
          ret.appendString(`if (${lookup} === undefined && ${lookupException} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_CLASS not found"; }\n`);
          ret.appendString(`if (${lookup} === undefined) { throw new ${lookupException}(); }\n`);
          if (i === 0) {
            ret.appendString(this.prepend);
          }
          ret.appendString(lookup);
        } else {
          ret.appendString("MethodSourceTranspiler-Unexpected");
        }
      } else if (child.get() instanceof Expressions.Dynamic) {
        const second = child.getChildren()[1];
        if (second.get() instanceof Expressions.FieldChain) {
          ret.appendChunk(traversal.traverse(second));
        } else if (second.get() instanceof Expressions.Constant) {
          ret.appendString(second.getFirstToken().getStr().replace(/\'/g, "").toLowerCase().replace("~", "$"));
        } else {
          ret.appendString("MethodSourceTranspiler-Unexpected");
        }
      } else if (child.get() instanceof Expressions.MethodName) {
        if (i === 0) {
          ret.appendString(this.prepend + "this.");
        }
        const methodName = child.concatTokens().toLowerCase().replace("~", "$");
        ret.append(methodName, child.getFirstToken().getStart(), traversal);
      } else if (child.concatTokens() === "=>") {
        ret.append(".", child.getFirstToken().getStart(), traversal);
      } else if (child.concatTokens() === "->") {
        if (ret.getCode() === "super") {
          ret.append(".", child, traversal);
        } else {
          ret.append(".get().", child, traversal);
        }
      } else if (child.get() instanceof Expressions.FieldChain) {
        if (i === 0) {
          ret.appendString(this.prepend);
        }
        ret.appendChunk(traversal.traverse(child));
      } else {
        ret.appendString("MethodSourceTranspiler-" + child.get().constructor.name + "-todo");
      }

    }

    return ret;
  }

}
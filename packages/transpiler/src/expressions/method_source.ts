/* eslint-disable max-len */
import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler} from ".";

export class MethodSourceTranspiler implements IExpressionTranspiler {
  private prepend: string;

  public constructor(prepend?: string) {
    this.prepend = (prepend || "") + "await ";
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    const children = node.getChildren();
    let call: string = "";

    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      const nextChild = children[i + 1];

      if (child.get() instanceof Expressions.ClassName) {
        call += traversal.lookupClassOrInterface(child.concatTokens(), child.getFirstToken());
      } else if (child.get() instanceof Expressions.Dynamic && nextChild?.concatTokens() === "=>") {
        const second = child.getChildren()[1];
        const illegalClass = traversal.lookupClassOrInterface("'CX_SY_DYN_CALL_ILLEGAL_CLASS'", child.getFirstToken(), true);
        // const illegalMethod = traversal.lookupClassOrInterface("'CX_SY_DYN_CALL_ILLEGAL_METHOD'", child.getFirstToken(), true);
        if (second.get() instanceof Expressions.FieldChain && second instanceof Nodes.ExpressionNode) {
          const t = new FieldChainTranspiler(true).transpile(second, traversal).getCode();

          call = traversal.lookupClassOrInterface(t, child.getFirstToken(), true);
          ret.appendString(`if (${call} === undefined) { if (${illegalClass} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_CLASS not found"; } else {throw new ${illegalClass}(); } }\n`);
        } else if (second.get() instanceof Expressions.Constant) {
          call = traversal.lookupClassOrInterface(second.getFirstToken().getStr(), child.getFirstToken(), true);
          ret.appendString(`if (${call} === undefined) { if (${illegalClass} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_CLASS not found"; } else {throw new ${illegalClass}(); } }\n`);

          /*
          const name = children[i + 2];
          if (name.get() instanceof Expressions.AttributeName) {
            const suffix = "." + name.concatTokens().toLowerCase().replace("~", "$");
            ret.appendString(`if (${call + suffix} === undefined && ${illegalMethod} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_METHOD not found"; }\n`);
            ret.appendString(`if (${call + suffix} === undefined) { throw new ${illegalMethod}(); }\n`);
          }
            */
        } else {
          ret.appendString("MethodSourceTranspiler-Unexpected");
        }
      } else if (child.get() instanceof Expressions.Dynamic) {
        const second = child.getChildren()[1];
//        const lookupException = traversal.lookupClassOrInterface("'CX_SY_DYN_CALL_ILLEGAL_METHOD'", child.getFirstToken(), true);
        if (second.get() instanceof Expressions.FieldChain && second instanceof Nodes.ExpressionNode) {
          if (call.endsWith(".")) {
            call = call.substring(0, call.length - 1);
          }
          if (call === "") {
            call = "this";
          }

          call = `abap.dynamicCallLookup(${call}, ${traversal.traverse(second).getCode()})`;
        } else if (second.get() instanceof Expressions.Constant) {
          if (call.endsWith(".")) {
            call = call.substring(0, call.length - 1);
          }
          if (call === "") {
            call = "this";
          }
          const methodName = second.getFirstToken().getStr().replace(/[\'\`]/g, "").toLowerCase().replace("~", "$").trimEnd();
          call = `abap.dynamicCallLookup(${call}, "${methodName}")`;
        } else {
          ret.appendString("MethodSourceTranspiler-Unexpected");
        }
        /*
        ret.appendString(`if (${call} === undefined && ${lookupException} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_METHOD not found"; }\n`);
        ret.appendString(`if (${call} === undefined) { throw new ${lookupException}(); }\n`);
        */
      } else if (child.get() instanceof Expressions.MethodName
          || child.get() instanceof Expressions.AttributeName) {
        if (i === 0) {
          this.prepend += "this.";
        }
        const nameToken = child.getFirstToken();
        const m = traversal.findMethodReference(nameToken, traversal.findCurrentScopeByToken(nameToken));
        if (m) {
          call += Traversal.escapeNamespace(m.name.toLowerCase().replace("~", "$"));
        } else {
          const methodName = Traversal.escapeNamespace(child.concatTokens().toLowerCase().replace("~", "$"));
          call += methodName;
        }
      } else if (child.concatTokens() === "=>") {
        call += ".";
      } else if (child.concatTokens() === "->") {
        if (ret.getCode() !== "super") {
          call += ".get()";
        }
        if (!(nextChild.get() instanceof Expressions.Dynamic)) {
          call += ".";
        }
      } else if (child.get() instanceof Expressions.FieldChain
          || child.get() instanceof Expressions.SourceField) {
        const nameToken = child.getFirstToken();
        const m = traversal.findMethodReference(nameToken, traversal.findCurrentScopeByToken(nameToken));
        if (i === 0 && m) {
          this.prepend += "this.";
        }

        call += traversal.traverse(child).getCode();
      } else {
        ret.appendString("MethodSourceTranspiler-" + child.get().constructor.name + "-todo");
      }
    }

    ret.appendString(this.prepend);
    ret.appendString(call);

    return ret;
  }

}
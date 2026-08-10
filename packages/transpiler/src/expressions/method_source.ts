/* eslint-disable max-len */
import {Nodes, Expressions, Tokens, Visibility} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler} from ".";

export class MethodSourceTranspiler implements IExpressionTranspiler {
  private prepend: string;
  private readonly privatePrefix: boolean;
  private readonly staticConstructorCall: boolean;

  /** @param staticConstructorCall set to false when building a reference to the method instead of calling it,
   *                               eg. SET HANDLER, where the dynamic method is the one to be registered */
  public constructor(prepend?: string, privatePrefix = false, staticConstructorCall = true) {
    this.prepend = (prepend || "") + "await ";
    this.privatePrefix = privatePrefix;
    this.staticConstructorCall = staticConstructorCall;
  }

  private static isMe(node: Nodes.ExpressionNode | Nodes.TokenNode | Nodes.StructureNode): boolean {
    return (node.get() instanceof Expressions.FieldChain || node.get() instanceof Expressions.SourceField)
      && node.concatTokens().toLowerCase() === "me";
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    const children = node.getChildren();
    let call: string = "";
    let bindConstructorCall = false;

    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      const nextChild = children[i + 1];

      if (child.get() instanceof Expressions.ClassName) {
        call += traversal.lookupClassOrInterface(child.concatTokens(), child.getFirstToken());
      } else if (child.get() instanceof Expressions.Dynamic && nextChild?.concatTokens() === "=>") {
        const second = child.getChildren()[1];
        const illegalClass = traversal.lookupClassOrInterface("'CX_SY_DYN_CALL_ILLEGAL_CLASS'", child.getFirstToken(), true);
        const illegalMethod = traversal.lookupClassOrInterface("'CX_SY_DYN_CALL_ILLEGAL_METHOD'", child.getFirstToken(), true);
        if (second.get() instanceof Expressions.FieldChain && second instanceof Nodes.ExpressionNode) {
          const t = new FieldChainTranspiler(true).transpile(second, traversal).getCode();

          call = traversal.lookupClassOrInterface(t, child.getFirstToken(), true);
          ret.appendString(`if (${call} === undefined) { if (${illegalClass} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_CLASS not found"; } else {throw new ${illegalClass}(); } }\n`);
        } else if (second.get() instanceof Expressions.Constant) {
          call = traversal.lookupClassOrInterface(second.getFirstToken().getStr(), child.getFirstToken(), true);
          ret.appendString(`if (${call} === undefined) { if (${illegalClass} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_CLASS not found"; } else {throw new ${illegalClass}(); } }\n`);

          const name = children[i + 2];
          if (name.get() instanceof Expressions.AttributeName) {
            const suffix = "." + name.concatTokens().toLowerCase().replace("~", "$");
            ret.appendString(`if (${call + suffix} === undefined && ${illegalMethod} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_METHOD not found"; }\n`);
            ret.appendString(`if (${call + suffix} === undefined) { throw new ${illegalMethod}(); }\n`);
          }
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
        const nameToken = child.getFirstToken();
        const scope = traversal.findCurrentScopeByToken(nameToken);
        const m = traversal.findMethodReference(nameToken, scope);
        // "me->name( )" has the same semantics as the unqualified "name( )"
        const viaMe = i === 2 && children.length === 3 && MethodSourceTranspiler.isMe(children[0]);
        if (this.staticConstructorCall && (i === 0 || viaMe)) {
          const prefix = traversal.constructorPrototypePrefix(nameToken, m?.def);
          if (prefix !== undefined) {
            this.prepend += prefix;
            call = ""; // discard the "this.me.get()." built for the viaMe case
            bindConstructorCall = true;
          } else if (i === 0) {
            this.prepend += "this.";
          }
        } else if (i === 0) {
          this.prepend += "this.";
        }
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
        const scope = traversal.findCurrentScopeByToken(nameToken);
        const m = traversal.findMethodReference(nameToken, scope);
        if (i === 0 && m) {
          const prefix = this.staticConstructorCall
            ? traversal.constructorPrototypePrefix(nameToken, m.def)
            : undefined;
          if (prefix !== undefined) {
            this.prepend += prefix;
            bindConstructorCall = true;
          } else {
            this.prepend += "this.";
          }
          if (this.privatePrefix && m.def.getVisibility() === Visibility.Private
              && m.def.isStatic() === false) { // todo: this is probably wrong?
            this.prepend += "#";
          }
        }

        call += traversal.traverse(child).getCode();
      } else if (child.get() instanceof Expressions.SourceFieldSymbol) {
        call += traversal.traverse(child).getCode();
      } else if (child.get() instanceof Expressions.ComponentName) {
        call += `["${child.concatTokens().toLowerCase()}"]`;
      } else if (child.get() instanceof Tokens.Dash) {
        call += '.get()';
      } else {
        ret.appendString("MethodSourceTranspiler-" + child.get().constructor.name + "-todo");
      }
    }

    if (bindConstructorCall) {
      call += ".bind(this)";
    }

    ret.appendString(this.prepend);
    ret.appendString(call);

    return ret;
  }

}

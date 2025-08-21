import {Expressions, Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {FieldLengthTranspiler, FieldOffsetTranspiler, TableExpressionTranspiler} from ".";
import {Traversal} from "../traversal";
import {FieldSymbolTranspiler} from "./field_symbol";
import {Chunk} from "../chunk";
import {FEATURE_FLAGS} from "../feature_flags";

export class FieldChainTranspiler implements IExpressionTranspiler {
  private readonly addGet: boolean;
  private addGetOffset: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, prefix = true, filename?: string, wrongScope = false): Chunk {
    let ret = new Chunk();
    const extra: string[] = [];
    let interfaceNameAdded = false;
    let context: abaplint.AbstractType | undefined = undefined;
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.SourceField
          || c.get() instanceof Expressions.Field) {
        const name = traversal.prefixAndName(c.getFirstToken(), filename).replace("~", "$");
        ret.append(Traversal.prefixVariable(Traversal.escapeNamespace(name)!), c, traversal);

        context = scope?.findVariable(c.getFirstToken().getStr())?.getType();
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.SourceFieldSymbol) {
        ret.appendChunk(new FieldSymbolTranspiler().transpile(c, traversal));
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ClassName) {
        const name = traversal.lookupClassOrInterface(c.getFirstToken().getStr(), c.getFirstToken());
        ret.append(name + ".", c, traversal);
        if (wrongScope === true && traversal.reg.getObject("INTF", c.getFirstToken().getStr())) {
          ret.append(Traversal.escapeNamespace(c.getFirstToken().getStr().toLocaleLowerCase())! + "$", c, traversal);
          interfaceNameAdded = true;
        }
      } else if (c.get() instanceof Expressions.AttributeName) {

        const interfaceName = traversal.isInterfaceAttribute(c.getFirstToken());
        let name = c.getFirstToken().getStr()!.toLowerCase();

        if (context instanceof abaplint.BasicTypes.ObjectReferenceType) {
          const cdef = traversal.findClassDefinition(context.getIdentifierName(), scope);
          const tokenName = c.getFirstToken().getStr();
          const attr = cdef?.getAttributes().findByName(tokenName);
          // Do not mark constants as private JS fields. Constants are exposed on instances via constructor copying.
          const constants = cdef?.getAttributes().getConstants() || [];
          const isConstant = constants.some(cons => cons.getName().toUpperCase() === tokenName.toUpperCase());
          if (FEATURE_FLAGS.PRIVATE_ATTRIBUTES === true
              && attr?.getVisibility() === abaplint.Visibility.Private
              && isConstant === false) {
            const id = scope?.getParent()?.getParent()?.getIdentifier();
            if (id?.stype === abaplint.ScopeType.ClassImplementation
                && cdef?.getName().toUpperCase() === id.sname.toUpperCase()) {
              name = "#" + name;
            } else {
              name = `FRIENDS_ACCESS_INSTANCE["${name}"]`;
            }
          }
        }

        if (prefix && interfaceName && name.startsWith(interfaceName) === false && interfaceNameAdded === false) {
          name = Traversal.escapeNamespace(name)!.replace("~", "$");
          name = Traversal.escapeNamespace(interfaceName) + "$" + name;
        } else {
          name = Traversal.escapeNamespace(name)!.replace("~", "$");
        }
        ret.append(name, c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Dereference) {
        ret.append(".dereference()", c, traversal);
      } else if (c.get() instanceof Expressions.ComponentName) {
        const name = c.getFirstToken().getStr().toLowerCase();
        if (name.match(/^\d/) || name.includes("/")) {
          ret.append(`["` + name + `"]`, c, traversal);
        } else {
          ret.append(`.` + name, c, traversal);
        }
      } else if (c instanceof Nodes.TokenNode) {
        const str = c.getFirstToken().getStr();
        if (str === "-") {
          ret.append(".get()", c, traversal);
        } else if (str === "->") {
          ret.append(".get().", c, traversal);
        }
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.FieldOffset) {
        extra.push("offset: " + new FieldOffsetTranspiler().transpile(c, traversal).getCode());
        this.addGetOffset = true;
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.FieldLength
          && c.concatTokens() !== "(*)") {
        extra.push("length: " + new FieldLengthTranspiler().transpile(c, traversal).getCode());
        this.addGetOffset = true;
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.TableExpression) {
        ret = new TableExpressionTranspiler().transpile(c, traversal, ret);
      }
    }

    if (this.addGetOffset) {
      let foo = extra.join(", ");
      if (foo !== "") {
        foo = "{" + foo + "}";
      }
      ret.appendString(".getOffset(" + foo + ")");  // todo, this will break
    }
    if (this.addGet) {
      ret.appendString(".get()");  // todo, this will break?
    }

    return ret;
  }

}
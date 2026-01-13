import {Nodes, Expressions} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";
import {UnknownTypesEnum} from "../types";

export class NewObjectTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const typeNameOrInfer = node.findDirectExpression(Expressions.TypeNameOrInfer);
    if (typeNameOrInfer === undefined) {
      throw new Error("NewObjectTranspiler: TypeNameOrInfer not found");
    }

    let para = "";
    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    if (parameters) {
      para = traversal.traverse(parameters).getCode();
    }

    let type = new TypeNameOrInfer().findTypeOrUndefined(typeNameOrInfer, traversal);
    if (type === undefined) {
      const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
      try {
        type = traversal.lookupType(typeNameOrInfer, scope);
      } catch {
        // ignore
      }
    }

    if (type === undefined) {
      if (traversal.options?.unknownTypes === UnknownTypesEnum.runtimeError) {
        ret.appendString(`(function() { throw new Error("Void type: ${typeNameOrInfer.concatTokens().toUpperCase()}") })()`);
        return ret;
      } else {
        throw new Error("NewObjectTranspiler, type not found: " + node.concatTokens() + ", " + traversal.getCurrentObject().getName() + " line " + node.getFirstToken().getStart().getRow());
      }
    } else if (type instanceof abaplint.BasicTypes.ObjectReferenceType) {
      if (node.getChildren()[3].get() instanceof abaplint.Expressions.Source) {
        // single default parameter
        const scope = traversal.findCurrentScopeByToken(typeNameOrInfer.getFirstToken());
        const cdef = traversal.findClassDefinition(type.getIdentifierName(), scope);
        const constr = this.findConstructor(cdef, scope);
        const pname = constr?.getParameters().getDefaultImporting()?.toLowerCase();
        para = `{${pname}: ${traversal.traverse(node.getChildren()[3]).getCode()}}`;
      }

      const clas = traversal.lookupClassOrInterface(type.getIdentifierName(), node.getFirstToken());
      ret.appendString(TranspileTypes.toType(type) + ".set(await (new " + clas + "()).constructor_(" + para + "))");
    } else {
      const source = node.findFirstExpression(abaplint.Expressions.Source);
      if (source === undefined) {
        throw new Error("NewObjectTranspiler: DataReference source not found");
      }
      const typeCode = TranspileTypes.toType(type);
      const sourceCode = traversal.traverse(source).getCode();
      ret.appendString("((() => {const r = new abap.types.DataReference(" + typeCode + "); r.assign(" + sourceCode + "); return r; })())");
    }

    return ret;
  }

  private findConstructor(cdef: abaplint.IClassDefinition | undefined, spag: abaplint.ISpaghettiScopeNode | undefined): any {
    let def = cdef;
    while (def !== undefined) {
      const method = def?.getMethodDefinitions().getByName("CONSTRUCTOR");
      if (method) {
        return method;
      }
      const name = def.getSuperClass();
      if (name) {
        def = spag?.findClassDefinition(name);
      } else {
        return undefined;
      }
    }
  }

}
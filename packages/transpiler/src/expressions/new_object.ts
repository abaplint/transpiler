import {Nodes, Expressions} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {UnknownTypesEnum} from "../types";
import {FieldAssignmentTranspiler} from "./field_assignment";

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
      ret.appendString(traversal.toType(type) + ".set(await (new " + clas + "()).constructor_(" + para + "))");
    } else {
      // a new data object with a copy of the value, never the source itself; async, as sources can await
      const typeCode = traversal.toType(type);
      const setCode = this.buildValue(node, type, traversal);
      ret.appendString("(await (async () => {const r = new abap.types.DataReference(" + typeCode + "); r.assign(" + typeCode + ");" + setCode + " return r; })())");
    }

    return ret;
  }

  private buildValue(node: Nodes.ExpressionNode, type: abaplint.AbstractType, traversal: Traversal): string {
    let ret = "";
    const parameters = node.findDirectExpression(Expressions.ParameterListS);
    if (parameters) {
      // NEW ty( a = 1 b = 2 ), components of a structure
      for (const p of parameters.findDirectExpressions(Expressions.ParameterS)) {
        const name = p.findDirectExpression(Expressions.ParameterName)!.concatTokens().toLowerCase();
        const source = traversal.traverse(p.findDirectExpression(Expressions.Source)).getCode();
        ret += ` r.getPointer().setField("${name}", ${source});`;
      }
      return ret;
    }

    const single = node.getChildren()[3];
    if (single instanceof Nodes.ExpressionNode && single.get() instanceof Expressions.Source) {
      return ` r.getPointer().set(${traversal.traverse(single).getCode()});`;
    } else if (!(type instanceof abaplint.BasicTypes.TableType)) {
      return ret;
    }

    // NEW tab( ( 1 ) ( a = 2 ) ), table lines
    const rowType = traversal.toType(type.getRowType());
    let row = "";
    for (const c of node.getChildren().slice(3)) {
      if (c instanceof Nodes.TokenNode) {
        if (c.getFirstToken().getStr() === ")" && row !== "") {
          ret += ` r.getPointer().appendThis(${rowType}${row});`;
          row = "";
        }
      } else if (c.get() instanceof Expressions.Source) {
        row += `.set(${traversal.traverse(c).getCode()})`;
      } else if (c.get() instanceof Expressions.FieldAssignment && c instanceof Nodes.ExpressionNode) {
        row += new FieldAssignmentTranspiler().transpile(c, traversal).getCode();
      }
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

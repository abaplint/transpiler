import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";
import {NewObjectTranspiler} from "./new_object";

/** THROW cx( ... ) in COND and SWITCH: raises the exception as RAISE EXCEPTION NEW cx( ... ) does, the result is a statement */
export class ThrowTranspiler implements IExpressionTranspiler {

  public transpile(node: abaplint.Nodes.ExpressionNode, traversal: Traversal): Chunk {
    if (node.findDirectTokenByText("MESSAGE")) {
      return new Chunk().append(`throw new Error("THROW MESSAGE, not supported, transpiler");`, node, traversal);
    }

    const classNameToken = node.findDirectExpression(abaplint.Expressions.ClassName)!.getFirstToken();
    let parameters = "";
    const list = node.findDirectExpression(abaplint.Expressions.ParameterListS);
    const single = node.findDirectExpression(abaplint.Expressions.Source);
    if (list) {
      parameters = traversal.traverse(list).getCode();
    } else if (single) {
      // the single parameter is the default importing parameter of the constructor
      const scope = traversal.findCurrentScopeByToken(classNameToken);
      const cdef = traversal.findClassDefinition(classNameToken.getStr(), scope);
      const name = NewObjectTranspiler.findConstructor(cdef, scope)?.getParameters().getDefaultImporting()?.toLowerCase();
      parameters = `{${name}: ${traversal.traverse(single).getCode()}}`;
    }

    const lookup = traversal.lookupClassOrInterface(classNameToken.getStr(), classNameToken);
    const id = UniqueIdentifier.get();
    return new Chunk().append(`const ${id} = await (new ${lookup}()).constructor_(${parameters});
${id}.EXTRA_CX = ${traversal.buildExtraCX(node)};
throw ${id};`, node, traversal);
  }

}

import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {UnknownTypesEnum} from "../types";

export class CreateObjectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();

    let para = "";
    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    if (parameters) {
      para = traversal.traverse(parameters).getCode();
    }

    let name = "";
    let directGlobal = false;
    let dynamic = node.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.ConstantString);
    if (dynamic) {
      name = dynamic.getFirstToken().getStr();
//      name = name.substring(1, name.length - 1);
    } else {
      dynamic = node.findDirectExpression(abaplint.Expressions.Dynamic)?.findFirstExpression(abaplint.Expressions.FieldChain);
      if (dynamic) {
        name = new FieldChainTranspiler(true).transpile(dynamic, traversal).getCode();
        directGlobal = true;
      }
    }

    if (name === "") {
      name = this.findClassName(node, traversal);
    }

    let ret = "";
    let clas = traversal.lookupClassOrInterface(name, node.getFirstToken(), directGlobal);
    const cx = traversal.lookupClassOrInterface("CX_SY_CREATE_OBJECT_ERROR", node.getFirstToken());
    if (dynamic) {
      const id = UniqueIdentifier.get();
      const internalName = UniqueIdentifier.get();
      ret += `let ${id} = abap.Classes["${traversal.buildPrefix()}"+${name}.trimEnd()];\n`;
      ret += `if (${id} === undefined) { ${id} = abap.Classes[${name}.trimEnd()]; }\n`;
      ret += `if (${id} === undefined && abap.Classes['KERNEL_INTERNAL_NAME'] !== undefined) {\n`;
      ret += `  const ${internalName} = await abap.Classes['KERNEL_INTERNAL_NAME'].rtti_to_internal({iv_rtti: ${name}});\n`;
      ret += `  ${id} = abap.Classes[${internalName}.get().trimEnd()];\n`;
      ret += `}\n`;
      ret += `if (${id} === undefined) { throw new ${cx}; }\n`;
      // ...and it has to be compatible with the STATIC type of the target.
      // A system raises CX_SY_CREATE_OBJECT_ERROR for a class that is not,
      // before the constructor runs; without this the class was assigned to
      // the reference whatever it was, and the mismatch surfaced later as a
      // javascript TypeError on the first member access - which no CATCH
      // takes, so a caller that guards its dynamic CREATE OBJECT the way the
      // documentation asks for was not guarded at all
      const staticName = this.findTargetTypeName(node, traversal);
      if (staticName !== undefined) {
        const staticType = traversal.lookupClassOrInterface(staticName, node.getFirstToken());
        ret += `abap.statements.checkCreateObjectType(${id}, ${staticType}, "${staticName.toUpperCase()}");\n`;
      }
      clas = id;
    }
    ret += target + ".set(await (new " + clas + "()).constructor_(" + para + "));";

    return new Chunk(ret);
  }

  /** The name of the target reference's STATIC type, or undefined when there
   * is nothing to check against - a generic `REF TO object`, or a target this
   * cannot resolve (which is reported by findClassName on the static path and
   * must not turn a working dynamic CREATE OBJECT into a transpile error
   * here). */
  private findTargetTypeName(node: abaplint.Nodes.StatementNode, traversal: Traversal): string | undefined {
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    if (scope === undefined) {
      return undefined;
    }
    const type = traversal.determineType(node, scope);
    if (type instanceof abaplint.BasicTypes.ObjectReferenceType) {
      return type.getIdentifierName();
    }
    return undefined;
  }

  private findClassName(node: abaplint.Nodes.StatementNode, traversal: Traversal) {
    const c = node.findDirectExpression(abaplint.Expressions.ClassName);
    if (c) {
      return c.concatTokens();
    }

    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    if (scope === undefined) {
      throw new Error("CreateObjectTranspiler, unable to lookup position");
    }

    const target = node.findDirectExpression(abaplint.Expressions.Target);
    if (target === undefined) {
      throw new Error(`CreateObjectTranspiler, target not found`);
    }

    const type = traversal.determineType(node, scope);
    if (type === undefined) {
      throw new Error(`CreateObjectTranspiler, target variable "${target?.concatTokens()}" not found in scope`);
    } else if (type instanceof abaplint.BasicTypes.GenericObjectReferenceType) {
      return "object";
    } else if (!(type instanceof abaplint.BasicTypes.ObjectReferenceType)) {
      if (traversal.options?.unknownTypes !== UnknownTypesEnum.runtimeError) {
        throw new Error(`CreateObjectTranspiler, target variable "${target?.concatTokens()}" not a object reference`);
      } else {
        return "RUNTIME_ERROR";
      }
    }

    return type.getIdentifierName();
  }

}

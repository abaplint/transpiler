import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class EndMethodTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const token = node.getFirstToken();

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("EndMethodTranspiler, scope not found");
    }

    let returning: string = "";
    returning += this.setSubrc(scope, traversal);

    const vars = scope.getData().vars;
    for (const n in vars) {
      const identifier = vars[n];
      if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        returning += "return " + Traversal.prefixVariable(n.toLowerCase()) + ";\n";
      }
    }

    const data = scope.getIdentifier();
    if (data.stype === abaplint.ScopeType.Method && data.sname.toLowerCase() === "constructor") {
      returning += "return this;\n";
    }

    return new Chunk().append(returning + "}", node, traversal);
  }

  private setSubrc(scope: abaplint.ISpaghettiScopeNode, traversal: Traversal): string {
    let methodName: string | undefined = undefined;
    if (scope?.getIdentifier().stype === abaplint.ScopeType.Method) {
      methodName = scope?.getIdentifier().sname;
    }
    let className: string | undefined = undefined;
    if (scope?.getParent()?.getIdentifier().stype === abaplint.ScopeType.ClassImplementation) {
      className = scope?.getParent()?.getIdentifier().sname;
    }

    if (methodName === undefined || className === undefined) {
      return "";
    }

    let methodDef: abaplint.IMethodDefinition | undefined = undefined;
    if (methodName.includes("~")) {
      const split = methodName.split("~");
      const classDef = traversal.findInterfaceDefinition(split[0], scope);
      methodDef = classDef?.getMethodDefinitions().getByName(split[1]);
    } else {
      const classDef = traversal.findClassDefinition(className, scope);
      methodDef = classDef?.getMethodDefinitions().getByName(methodName);
    }

    if (methodDef && methodDef.getExceptions().length > 0) {
      return "abap.builtin.sy.get().subrc.set(0);\n";
    }

    return "";
  }

}
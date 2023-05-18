/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ClassImplementationTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const token = node.findFirstExpression(abaplint.Expressions.ClassName)!.getFirstToken();

    const def = traversal.getClassDefinition(token);

    let ret = "class " + Traversal.escapeNamespace(token.getStr().toLowerCase());

    if (token.getStr().toLowerCase() === "cx_root") {
      // special case for exceptions
      ret += " extends Error";
    } else if (def?.getSuperClass()) {
      ret += " extends " + Traversal.escapeNamespace(def?.getSuperClass()?.toLowerCase());
    }

    const scope = traversal.findCurrentScopeByToken(token);

    return new Chunk().append(ret + ` {
static INTERNAL_TYPE = 'CLAS';
static INTERNAL_NAME = '${traversal.buildInternalName(token.getStr(), def)}';
static IMPLEMENTED_INTERFACES = [${this.findImplementedByClass(traversal, def, scope).map(e => `"` + e.toUpperCase() + `"`).join(",")}];
static ATTRIBUTES = {${traversal.buildAttributes(def, scope).join(",\n")}};
static METHODS = {${traversal.buildMethods(def, scope).join(",\n")}};`, node, traversal);
  }

  private findImplementedInterface(traversal: Traversal, def?: abaplint.IInterfaceDefinition, scope?: abaplint.ISpaghettiScopeNode): string[] {
    if (def === undefined || scope === undefined) {
      return [];
    }

    const list = def.getImplementing().map(i => i.name.toUpperCase());

    for (const i of def.getImplementing()) {
      const idef = traversal.findInterfaceDefinition(i.name, scope);
      list.push(...this.findImplementedInterface(traversal, idef, scope));
    }

    return list;
  }

  private findImplementedByClass(traversal: Traversal, def?: abaplint.IClassDefinition, scope?: abaplint.ISpaghettiScopeNode): string[] {
    if (def === undefined || scope === undefined) {
      return [];
    }

    const list = def.getImplementing().map(i => i.name.toUpperCase());

    for (const i of def.getImplementing()) {
      const idef = traversal.findInterfaceDefinition(i.name, scope);
      list.push(...this.findImplementedInterface(traversal, idef, scope));
    }

    let sup = def.getSuperClass();
    while (sup !== undefined) {
      const sdef = traversal.findClassDefinition(sup, scope);
      list.push(...this.findImplementedByClass(traversal, sdef, scope));
      sup = sdef?.getSuperClass();
    }

    return list;
  }

}
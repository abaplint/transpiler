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
static IMPLEMENTED_INTERFACES = [${this.findImplemented(traversal, def, scope).map(e => `"` + e.toUpperCase() + `"`).join(",")}];`, node, traversal);
  }

  private findImplemented(traversal: Traversal, def?: abaplint.IClassDefinition, scope?: abaplint.ISpaghettiScopeNode): string[] {
    if (def === undefined || scope === undefined) {
      return [];
    }

    const list = def.getImplementing().map(i => i.name.toUpperCase());

// todo, interfaces implemented by interfaces?

    let sup = def.getSuperClass();
    while (sup !== undefined) {
      const sdef = traversal.findClassDefinition(sup, scope);
      list.push(...this.findImplemented(traversal, sdef, scope));
      sup = sdef?.getSuperClass();
    }

    return list;
  }

}
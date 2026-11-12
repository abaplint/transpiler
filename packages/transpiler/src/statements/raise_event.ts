import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ParameterListSTranspiler} from "../expressions";

export class RaiseEventTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const eventName = Traversal.escapeNamespace(node.findFirstExpression(abaplint.Expressions.EventName)?.concatTokens())?.toLowerCase().replace("~", "$");

    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    let extra = "";
    if (parameters) {
      extra = "," + new ParameterListSTranspiler().transpile(parameters, traversal).getCode();
    }

    // a static event: handlers may only react when the class of this statement is theirs, it is passed along
    const def = traversal.getClassDefinition(node.getFirstToken());
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    let search: abaplint.IClassDefinition | undefined = def;
    let isStatic = false;
    while (search !== undefined) {
      const event = search.getEvents().find(e => e.getName().toLowerCase() === eventName);
      if (event !== undefined) {
        isStatic = event.isStatic();
        break;
      }
      search = search.getSuperClass() ? traversal.findClassDefinition(search.getSuperClass(), scope) : undefined;
    }
    if (isStatic === true && def !== undefined) {
      extra = (extra === "" ? ", undefined" : extra) + ", " + Traversal.escapeNamespace(def.getName().toLowerCase());
    }

    return new Chunk().append(`await abap.statements.raiseEvent(this.${eventName}, this.me${extra});`, node, traversal);
  }

}
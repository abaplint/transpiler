import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {MethodSourceTranspiler, SourceTranspiler} from "../expressions";

export class SetHandlerTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const methods: string[] = [];
    let eventClass: string | undefined = undefined;
    let eventName: string | undefined = undefined;

    for (const m of node.findDirectExpressions(abaplint.Expressions.MethodSource)) {
      methods.push(new MethodSourceTranspiler().transpile(m, traversal).getCode().replace("await ", "") + ".bind(this)");

      if (eventClass === undefined) {
        const nameToken = m.getLastToken();
        const scope = traversal.findCurrentScopeByToken(nameToken);
        const method = traversal.findMethodReference(nameToken, scope);
        if (method?.def.isEventHandler() !== true) {
          throw new Error(`SetHandlerTranspiler: Method "${nameToken.getStr()}" is not an event handler`);
        }

        eventName = method.def.getEventName();
        eventClass = this.findEventClass(method.def, traversal, scope);
      }
    }

    let f: string | undefined = undefined;
    const forExpression = node.findExpressionAfterToken("FOR");
    if (forExpression instanceof abaplint.Expressions.Source) {
      f = new SourceTranspiler().transpile(forExpression, traversal).getCode();
    } else {
      f = `"ALL"`;
    }

    let activation: string = "";
    const activationExpression = node.findExpressionAfterToken("ACTIVATION");
    if (activationExpression) {
      activation = ", " + new SourceTranspiler().transpile(activationExpression, traversal).getCode();
    }

    return new Chunk().append(`abap.statements.setHandler({EVENT_NAME: "${eventName}", EVENT_CLASS: "${eventClass}"}, [${methods.join(",")}], ${f}${activation});`, node, traversal);
  }

///////////////////////////////////////////////////////////////////////

  private findEventClass(mdef: abaplint.Types.MethodDefinition,
                         traversal: Traversal,
                         scope: abaplint.ISpaghettiScopeNode | undefined): string {

    let def: abaplint.IClassDefinition | abaplint.IInterfaceDefinition | undefined
      = traversal.findClassDefinition(mdef.getEventClass(), scope);
    if (def === undefined) {
      def = traversal.findInterfaceDefinition(mdef.getEventClass()!, scope);
    }

    // look through super classes
    while (def?.getSuperClass() !== undefined) {
      if (def.getEvents().find(e => e.getName().toUpperCase() === mdef.getEventName()?.toUpperCase())) {
        break;
      }
      def = traversal.findClassDefinition(def.getSuperClass(), scope);
    }

    if (def === undefined) {
      throw new Error(`SetHandlerTranspiler: findEventClass, class or interface "${mdef.getEventClass()}" not found`);
    }
    return traversal.buildInternalName(def.getName(), def);
  }

}
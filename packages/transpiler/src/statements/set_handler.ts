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
    let forClass = "";

    for (const m of node.findDirectExpressions(abaplint.Expressions.MethodSource)) {
      // the handler method is registered as a reference, it is dispatched dynamically when the event is raised,
      // so no static binding even if the SET HANDLER is inside a constructor
      const msource = new MethodSourceTranspiler("", true, false).transpile(m, traversal).getCode().replace("await ", "");
      const lastDot = msource.lastIndexOf(".");
      const receiver = lastDot === -1 ? "this" : msource.substring(0, lastDot);
      methods.push(`{method: ${msource}, receiver: ${receiver}}`);

      if (eventClass === undefined) {
        const nameToken = m.getLastToken();
        const scope = traversal.findCurrentScopeByToken(nameToken);
        const method = traversal.findMethodReference(nameToken, scope);
        if (method?.def.isEventHandler() !== true) {
          // eg. the class of the handler is not known
          const text = `SET HANDLER, ${nameToken.getStr()} is not a known event handler, transpiler`;
          return new Chunk().append(`throw new Error("${text}");`, node, traversal);
        }

        eventName = method.def.getEventName();
        eventClass = this.findEventClass(method.def, traversal, scope);
        if (eventClass === undefined) {
          const text = `SET HANDLER, class or interface ${method.def.getEventClass()} not known, transpiler`;
          return new Chunk().append(`throw new Error("${text}");`, node, traversal);
        }
        forClass = this.findForClass(method.def, eventClass, traversal, scope);
      }
    }

    let f: string | undefined = undefined;
    const forExpression = node.findExpressionAfterToken("FOR");
    if (forExpression?.get() instanceof abaplint.Expressions.Source) {
      f = new SourceTranspiler().transpile(forExpression, traversal).getCode();
    } else {
      f = `"ALL"`;
    }

    let activation: string = "";
    const activationExpression = node.findExpressionAfterToken("ACTIVATION");
    if (activationExpression) {
      activation = ", " + new SourceTranspiler().transpile(activationExpression, traversal).getCode();
    }

    return new Chunk().append(`abap.statements.setHandler({EVENT_NAME: "${eventName}", EVENT_CLASS: "${eventClass}"${forClass}}, [${methods.join(",")}], ${f}${activation});`, node, traversal);
  }

///////////////////////////////////////////////////////////////////////

  /** a handler FOR EVENT ... OF a subclass of the class declaring the event only reacts to that subclass */
  private findForClass(mdef: abaplint.Types.MethodDefinition, eventClass: string,
                       traversal: Traversal, scope: abaplint.ISpaghettiScopeNode | undefined): string {
    const own = traversal.findClassDefinition(mdef.getEventClass(), scope);
    if (own === undefined || traversal.buildInternalName(own.getName(), own) === eventClass) {
      return "";
    }
    let def: abaplint.IClassDefinition | undefined = own;
    let event = undefined;
    while (def !== undefined && event === undefined) {
      event = def.getEvents().find(e => e.getName().toUpperCase() === mdef.getEventName()?.toUpperCase());
      def = def.getSuperClass() ? traversal.findClassDefinition(def.getSuperClass(), scope) : undefined;
    }
    return `, FOR_CLASS: "${traversal.buildInternalName(own.getName(), own)}", STATIC: ${event?.isStatic() === true}`;
  }

  private findEventClass(mdef: abaplint.Types.MethodDefinition,
                         traversal: Traversal,
                         scope: abaplint.ISpaghettiScopeNode | undefined): string | undefined {

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
      return undefined;
    }
    return traversal.buildInternalName(def.getName(), def);
  }

}

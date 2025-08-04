import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {MethodSourceTranspiler, SourceTranspiler} from "../expressions";

export class SetHandlerTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const methods: string[] = [];
    let className: string | undefined = undefined;
    let eventName: string | undefined = undefined;

    for (const m of node.findDirectExpressions(abaplint.Expressions.MethodSource)) {
      methods.push(new MethodSourceTranspiler().transpile(m, traversal).getCode().replace("await ", ""));

      if (className === undefined) {
        const nameToken = m.getFirstToken();
        const scope = traversal.findCurrentScopeByToken(nameToken);
        const method = traversal.findMethodReference(nameToken, scope);
        if (method?.def.isEventHandler() !== true) {
          throw new Error(`Transpiler: Method "${nameToken.getStr()}" is not an event handler`);
        }

        const def = traversal.findClassDefinition(method.def.getEventClass(), scope);
        if (def === undefined) {
          throw new Error(`Transpiler: Method "${nameToken.getStr()}" is not an event handler`);
        }
        eventName = method.def.getEventName();

        for (const event of def.getEvents()) {
          if (event.getName() === eventName) {
            className = traversal.buildInternalName(def.getName(), def);
            break;
          }
        }
        if (className === undefined) {
          throw new Error(`Transpiler: Event "${eventName}" not found in class "${def.getName()}"`);
        }
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

    return new Chunk().append(`abap.statements.setHandler({EVENT_NAME: "${eventName}", EVENT_CLASS: "${className}"}, [${methods.join(",")}], ${f}${activation});`, node, traversal);
  }

}
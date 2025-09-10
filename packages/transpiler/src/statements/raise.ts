import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";

export class RaiseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const classNameToken = node.findFirstExpression(abaplint.Expressions.ClassName)?.getFirstToken();
    const className = classNameToken?.getStr();
    if (className === undefined) {
      let s = node.findFirstExpression(abaplint.Expressions.SimpleSource2);
      if (s === undefined) {
        s = node.findFirstExpression(abaplint.Expressions.Source);
      }

      if (s === undefined) {
        const name = node.findFirstExpression(abaplint.Expressions.ExceptionName)?.concatTokens().toLowerCase();
        return new Chunk().append(`throw new abap.ClassicError({classic: "${name}"});`, node, traversal);
      }
      const sCode = new SourceTranspiler(true).transpile(s, traversal).getCode();
      return new Chunk().append(`throw ${sCode};`, node, traversal);
    }

    let p = "";
    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    if (parameters) {
      p = traversal.traverse(parameters).getCode();
    }
    
    // Check for MESSAGE clause in RAISE EXCEPTION TYPE <class> MESSAGE <message>
    const messageOptions: string[] = [];
    const messageSource = node.findFirstExpression(abaplint.Expressions.MessageSource);
    if (messageSource) {
      const id = messageSource.findExpressionAfterToken("ID");
      if (id) {
        messageOptions.push("id: " + traversal.traverse(id).getCode());
      }

      const cla = messageSource.findDirectExpression(abaplint.Expressions.MessageClass);
      if (cla) {
        messageOptions.push("id: \"" + cla.concatTokens() + "\"");
      }

      const type = messageSource.findExpressionAfterToken("TYPE");
      if (type) {
        messageOptions.push("type: " + traversal.traverse(type).getCode());
      }

      const number = messageSource.findExpressionAfterToken("NUMBER");
      if (number) {
        messageOptions.push("number: " + traversal.traverse(number).getCode());
      }

      const typeAndNumber = messageSource.findDirectExpression(abaplint.Expressions.MessageTypeAndNumber);
      if (typeAndNumber) {
        const str = typeAndNumber.getFirstToken().getStr();
        messageOptions.push("number: \"" + str.substr(1, 3) + "\"");
        messageOptions.push("type: \"" + str.substr(0, 1).toUpperCase() + "\"");
      }

      // Handle WITH clause for message parameters
      const w: string[] = [];
      let withs = false;
      for (const c of node.getChildren()) {
        if (c.getFirstToken().getStr().toUpperCase() === "WITH") {
          withs = true;
        } else if (withs === true && c instanceof abaplint.Nodes.ExpressionNode) {
          // For each expression after WITH, traverse it
          w.push(traversal.traverse(c).getCode());
        } else if (withs === true && c.getFirstToken().getStr() === ".") {
          // Stop at the end of statement  
          break;
        }
      }
      if (w.length > 0) {
        messageOptions.push("with: [" + w.join(",") + "]");
      }
    }
    
    const extra = `{"INTERNAL_FILENAME": "${traversal.getFilename()}","INTERNAL_LINE": ${node.getStart().getRow()}}`;
    const lookup = traversal.lookupClassOrInterface(classNameToken?.getStr(), classNameToken);
    const id = UniqueIdentifier.get();

    let code = `const ${id} = await (new ${lookup}()).constructor_(${p});\n${id}.EXTRA_CX = ${extra};`;
    
    // If we have message information, set it on the exception
    if (messageOptions.length > 0) {
      code += `\n${id}.MESSAGE_INFO = {${messageOptions.join(", ")}};`;
    }
    
    code += `\nthrow ${id};`;

    return new Chunk().append(code, node, traversal);
  }

}
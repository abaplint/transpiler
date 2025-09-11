import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";

export class RaiseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    if (node.findDirectTokenByText("RESUMABLE")) {
      throw new Error("RaiseTranspiler, RESUMABLE not implemented");
    }

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
    const extra = `{"INTERNAL_FILENAME": "${traversal.getFilename()}","INTERNAL_LINE": ${node.getStart().getRow()}}`;
    const lookup = traversal.lookupClassOrInterface(classNameToken?.getStr(), classNameToken);
    const id = UniqueIdentifier.get();

    const messageSource = node.findDirectExpression(abaplint.Expressions.MessageSource);
    let pre = "";
    let post = "";
    if (messageSource) {
      let msgid = messageSource.findDirectExpression(abaplint.Expressions.MessageClass
        )?.concatTokens().toUpperCase();
      if (msgid === undefined) {
        msgid = traversal.traverse(messageSource.findExpressionAfterToken("ID")).getCode();
      } else {
        msgid = `'${msgid}'`;
      }

      let msgno = messageSource.findDirectExpression(abaplint.Expressions.MessageTypeAndNumber
        )?.concatTokens().substring(1);
      if (msgno === undefined) {
        msgno = traversal.traverse(messageSource.findExpressionAfterToken("NUMBER")).getCode();
      } else {
        msgno = `'${msgno}'`;
      }

      let msgty = messageSource.findDirectExpression(abaplint.Expressions.MessageTypeAndNumber
        )?.concatTokens().substring(0, 1).toUpperCase();
      if (msgty === undefined) {
        msgty = traversal.traverse(messageSource.findExpressionAfterToken("TYPE")).getCode();
      } else {
        msgty = `'${msgty}'`;
      }

      const textid = UniqueIdentifier.get();
      pre = `const ${textid} = new abap.types.Structure({
"msgid": new abap.types.Character(20, {}),
"msgno": new abap.types.Numc({length: 3}),
"attr1": new abap.types.Character(255, {}),
"attr2": new abap.types.Character(255, {}),
"attr3": new abap.types.Character(255, {}),
"attr4": new abap.types.Character(255, {})}, "SCX_T100KEY", "SCX_T100KEY", {}, {});
${textid}.get().msgid.set(${msgid});
${textid}.get().msgno.set(${msgno});
${textid}.get().attr1.set('IF_T100_DYN_MSG~MSGV1');
${textid}.get().attr2.set('IF_T100_DYN_MSG~MSGV2');
${textid}.get().attr3.set('IF_T100_DYN_MSG~MSGV3');
${textid}.get().attr4.set('IF_T100_DYN_MSG~MSGV4');
`;
      if (p === "") {
        p = `{"textid": ${textid}}`;
      } else {
        p = `{...${p}, "textid": ${textid}}`;
      }

      post = `\n${id}.if_t100_dyn_msg$msgty?.set(${msgty});`;

      let count = 1;
      for (const w of node.findDirectExpression(abaplint.Expressions.RaiseWith)?.findDirectExpressions(abaplint.Expressions.Source) || []) {
        post += `\n${id}.if_t100_dyn_msg$msgv${count}?.set(${traversal.traverse(w).getCode()});`;
        count++;
      }
    }

    let throwError = `throw ${id};`;
    if (node.findDirectTokenByText("SHORTDUMP")) {
      throwError = `throw new Error("short dump");`;
    }

    return new Chunk().append(pre + `const ${id} = await (new ${lookup}()).constructor_(${p});
${id}.EXTRA_CX = ${extra};${post}
${throwError}`, node, traversal);
  }

}
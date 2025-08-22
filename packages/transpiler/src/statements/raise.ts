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
    const extra = `{"INTERNAL_FILENAME": "${traversal.getFilename()}","INTERNAL_LINE": ${node.getStart().getRow()}}`;
    const lookup = traversal.lookupClassOrInterface(classNameToken?.getStr(), classNameToken);
    const id = UniqueIdentifier.get();

    return new Chunk().append(`const ${id} = await (new ${lookup}()).constructor_(${p});
${id}.EXTRA_CX = ${extra};
throw ${id};`, node, traversal);
  }

}
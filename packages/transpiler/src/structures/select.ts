import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {SelectTranspiler as SelectStatementTranspiler} from "../statements/select";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";
import {SQLSourceTranspiler, SQLTargetTranspiler} from "../expressions";

export class SelectTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    const selectStatement = node.findFirstStatement(abaplint.Statements.SelectLoop);
    if (selectStatement === undefined) {
      throw "Structure, select loop not found";
    }
    const concat = selectStatement.concatTokens().toUpperCase();
    const from = selectStatement.findFirstExpression(abaplint.Expressions.SQLFromSource)?.concatTokens().toUpperCase();
    const intoName = new SQLTargetTranspiler().transpile(
      selectStatement.findFirstExpression(abaplint.Expressions.SQLTarget)!, traversal).getCode();

    // note: this implementation SELECTs everything into memory, which might be bad, and sometimes not correct
    const targetName = UniqueIdentifier.get();
    const loopName = UniqueIdentifier.get();
    ret.appendString(`let ${targetName} = new abap.types.Table(abap.DDIC["${from}"].type);\n`);
    ret.appendChunk(new SelectStatementTranspiler().transpile(selectStatement, traversal, targetName));

    // todo: optimize, it should do real streaming?
    const packageSize = node.findFirstExpression(abaplint.Expressions.SelectLoop)?.findExpressionAfterToken("SIZE");
    if (packageSize) {
      const getSize = new SQLSourceTranspiler().transpile(packageSize, traversal).getCode() + ".get()";
      ret.appendString(`if (${targetName}.array().length > ${getSize}) {
  throw new Error("PACKAGE SIZED loop larger than package size not supported");
};
abap.statements.append({source: ${targetName}, target: ${intoName}, lines: true});
{\n`);
    } else if (concat.includes(" INTO CORRESPONDING FIELDS OF ")) {
      ret.appendString(`\nfor (const ${loopName} of ${targetName}.array()) {\n`);
      ret.appendString(`abap.statements.moveCorresponding(${loopName}, ${intoName});\n`);
    } else {
      ret.appendString(`\nfor (const ${loopName} of ${targetName}.array()) {\n`);
      ret.appendString(`${intoName}.set(${loopName});\n`);
    }

    const body = node.findDirectStructure(abaplint.Structures.Body);
    if (body) {
      ret.appendChunk(traversal.traverse(body));
    }

    ret.appendString("}\n");

    return ret;
  }

}
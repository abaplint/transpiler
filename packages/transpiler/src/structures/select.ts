import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {SelectTranspiler as SelectStatementTranspiler} from "../statements/select";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";

export class SelectTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    const selectStatement = node.findFirstStatement(abaplint.Statements.SelectLoop);
    if (selectStatement === undefined) {
      throw "Structure, select loop not found";
    }
    const from = selectStatement.findFirstExpression(abaplint.Expressions.SQLFromSource)?.concatTokens().toUpperCase();
    const intoName = selectStatement.findFirstExpression(abaplint.Expressions.SQLTarget)?.concatTokens();

    // note: this implementation SELECTs everything into memory, which might be bad, and sometimes not correct
    const targetName = UniqueIdentifier.get();
    const loopName = UniqueIdentifier.get();
    ret.appendString(`let ${targetName} = new abap.types.Table(abap.DDIC["${from}"].type);\n`);
    ret.appendChunk(new SelectStatementTranspiler().transpile(selectStatement, traversal, targetName));
    ret.appendString(`\nfor (const ${loopName} of ${targetName}.array()) {\n`);
    ret.appendString(`${intoName?.replace("-", ".get().")}.set(${loopName});\n`);

    const body = node.findDirectStructure(abaplint.Structures.Body);
    if (body) {
      ret.appendChunk(traversal.traverse(body));
    }

    ret.appendString("}\n");

    return ret;
  }

}
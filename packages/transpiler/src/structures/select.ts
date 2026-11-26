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
    const sTarget = selectStatement.findFirstExpression(abaplint.Expressions.SQLTarget);

    let intoName: string = "";
    if (sTarget === undefined) {
      intoName = from!.toLowerCase();
    } else {
      intoName = new SQLTargetTranspiler().transpile(sTarget, traversal).getCode();
    }

    // note: this implementation SELECTs everything into memory, which might be bad, and sometimes not correct
    const targetName = UniqueIdentifier.get();
    const loopName = UniqueIdentifier.get();
    // the sources of the loop's own FROM, not those of a subquery in its WHERE
    const sources = selectStatement.findFirstExpression(abaplint.Expressions.SQLFrom)
      ?.findAllExpressions(abaplint.Expressions.SQLFromSource) ?? [];
    const aliased = sources[0]?.findDirectExpression(abaplint.Expressions.SQLAsName) !== undefined;
    const table = sources[0]?.findDirectExpression(abaplint.Expressions.DatabaseTable)?.concatTokens().toUpperCase();
    const intoList = selectStatement.findFirstExpression(abaplint.Expressions.SQLIntoList);
    const loopOptions: string[] = [];
    let assign: string;
    if (intoList) {
      // INTO (a, b, c): the buffer row has one component per target, filled
      // from the columns left to right, and every target is assigned per row
      const targets = intoList.findDirectExpressions(abaplint.Expressions.SQLTarget)
        .map(t => new SQLTargetTranspiler().transpile(t, traversal).getCode());
      const components = targets.map((t, i) => `"c${i}": ${this.cloneOf(t)}`);
      ret.appendString(`let ${targetName} = new abap.types.Table(new abap.types.Structure({${components.join(", ")}}));\n`);
      loopOptions.push("positional: true");
      assign = targets.map((t, i) => `${t}.set(${loopName}.get().c${i});\n`).join("");
    } else {
      if (aliased && sources.length === 1 && table !== undefined) {
        // "tab AS a" is no DDIC name, the row type is that of the table
        ret.appendString(`let ${targetName} = new abap.types.Table(abap.DDIC["${table}"].type());\n`);
      } else if (aliased && sTarget !== undefined) {
        // a join that starts with "tab AS a" has no row type to look up, the buffer
        // takes the work area's; without CORRESPONDING it is filled left to right
        ret.appendString(`let ${targetName} = new abap.types.Table(${this.cloneOf(intoName)});\n`);
        if (concat.includes(" INTO CORRESPONDING FIELDS OF ") === false) {
          loopOptions.push("positional: true");
        }
      } else {
        ret.appendString(`let ${targetName} = new abap.types.Table(abap.DDIC["${from}"].type());\n`);
      }
      if (concat.includes(" INTO CORRESPONDING FIELDS OF ")) {
        assign = `abap.statements.moveCorresponding(${loopName}, ${intoName});\n`;
      } else {
        assign = `${intoName}.set(${loopName});\n`;
      }
    }
    const selectHead = new SelectStatementTranspiler().transpile(selectStatement, traversal, targetName, loopOptions);
    ret.appendChunk(selectHead.ensureStartMapping(selectStatement, traversal));

    // todo: optimize, it should do real streaming?
    const packageSize = selectStatement.findFirstExpression(abaplint.Expressions.SQLPackageSize)
      ?.findFirstExpression(abaplint.Expressions.SQLSource);
    if (packageSize) {
      const getSize = new SQLSourceTranspiler().transpile(packageSize, traversal).getCode() + ".get()";
      ret.appendString(`if (${targetName}.array().length > ${getSize}) {
  throw new Error("PACKAGE SIZED loop larger than package size not supported");
};
abap.statements.append({source: ${targetName}, target: ${intoName}, lines: true});
{\n`);
    } else {
      // every pass sets sy-dbcnt to the number of rows passed so far and sy-subrc
      // to 0; ENDSELECT sets both once more when the rows run out, which an EXIT
      // does not reach
      const countName = UniqueIdentifier.get();
      ret.appendString(`\nlet ${countName} = 0;
for (;;) {
const ${loopName} = ${targetName}.array()[${countName}];
if (${loopName} === undefined) {
abap.builtin.sy.get().dbcnt.set(${countName});
abap.builtin.sy.get().subrc.set(${countName} > 0 ? 0 : 4);
break;
}
abap.builtin.sy.get().dbcnt.set(++${countName});
abap.builtin.sy.get().subrc.set(0);\n`);
      ret.appendString(assign);
    }

    const body = node.findDirectStructure(abaplint.Structures.Body);
    if (body) {
      ret.appendChunk(traversal.traverse(body));
    }

    ret.appendString("}\n");

    return ret;
  }

  // a fresh value of the target's type, also when the target is a field symbol
  private cloneOf(code: string): string {
    return `(${code} instanceof abap.types.FieldSymbol ? ${code}.getPointer() : ${code}).clone()`;
  }

}

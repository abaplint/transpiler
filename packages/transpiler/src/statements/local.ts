import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

/** LOCAL f (obsolete): f gets its value back when the FORM or function module ends, see FormTranspiler */
export class LocalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const field = node.findDirectExpression(abaplint.Expressions.FieldSub);
    if (field === undefined) {
      throw new Error("LocalTranspiler, field not found");
    }
    // a data object, or a component of a structure (LOCAL struc-comp)
    const first = field.getFirstToken();
    const variable = traversal.findCurrentScopeByToken(first)?.findVariable(first.getStr());
    const [, ...components] = field.concatTokens().toLowerCase().split("-");
    const name = traversal.prefixAndName(first, variable?.getFilename()).replace("~", "$")
      + components.map(c => ".get()." + c).join("");
    const code = `{ const saved = ${name}.clone(); ${LocalTranspiler.LIST}.push(() => ${name}.set(saved)); }`;
    return new Chunk().append(code, node, traversal);
  }

  /** the variable of a procedure that holds what LOCAL puts back */
  public static readonly LIST = "localRestore";

  /** code around the body of a procedure with LOCAL: the values are put back however the procedure ends */
  public static wrap(node: abaplint.Nodes.StructureNode): {open: string, close: string} | undefined {
    if (node.findFirstStatement(abaplint.Statements.Local) === undefined) {
      return undefined;
    }
    return {
      open: `const ${LocalTranspiler.LIST} = [];\ntry {\n`,
      close: `} finally {\nfor (const restore of ${LocalTranspiler.LIST}.reverse()) {\nrestore();\n}\n}\n`,
    };
  }

}

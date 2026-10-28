import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {FieldSymbolTranspiler as Expr} from "../expressions/field_symbol";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class FieldSymbolTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode | abaplint.Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const name = node.findDirectExpression(abaplint.Expressions.FieldSymbol);

    const token = name?.getFirstToken();
    if (token === undefined) {
      throw new Error("FieldSymbolTranspiler, token not found");
    }

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("FieldSymbolTranspiler, scope not found");
    }

    const found = scope.findVariable(token.getStr());
    if (found === undefined) {
      throw new Error("FieldSymbolTranspiler, var not found, \"" + token.getStr() + "\"");
    }

    let type = found.getType();
    // abaplint types TYPE STANDARD TABLE, SORTED TABLE, HASHED TABLE, INDEX TABLE and
    // ANY TABLE alike, a generic table without a primary key; the kind is in the statement,
    // and the runtime needs it to refuse an ASSIGN of a table of another kind
    const typeTable = node.findDirectExpression(abaplint.Expressions.TypeTable);
    if (typeTable !== undefined
        && type instanceof abaplint.BasicTypes.TableType
        && type.getOptions().primaryKey === undefined) {
      const kind = /^TYPE (STANDARD|SORTED|HASHED|INDEX|ANY) TABLE$/i.exec(typeTable.concatTokens());
      if (kind !== null) {
        const options: abaplint.BasicTypes.ITableOptions = {
          ...type.getOptions(),
          primaryKey: {
            name: "primary_key",
            type: kind[1].toUpperCase() as abaplint.BasicTypes.TableAccessType,
            keyFields: [],
            isUnique: false,
          },
        };
        type = new abaplint.BasicTypes.TableType(type.getRowType(), options, type.getQualifiedName());
      }
    }

    if (name) {
      return new Chunk()
        .appendString("let ")
        .appendString(new Expr().transpile(name, traversal).getCode())
        .appendString(" = new abap.types.FieldSymbol(" + traversal.toType(type) + ");");
    }

    throw new Error("FieldSymbolTranspiler, name not found");
  }

}
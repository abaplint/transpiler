import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DoTranspiler implements IStatementTranspiler {
  private readonly syIndexBackup: string;

  public constructor(syIndexBackup: string) {
    this.syIndexBackup = syIndexBackup;
  }

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const found = node.findFirstExpression(abaplint.Expressions.Source);
    if (found) {
      // the count is converted to i, like any numeric expression position: an
      // f count read with get( ) gives its text 3,0000000000000000E+00, which
      // no loop counter compares with, and a p count of 2.4 means 2 passes
      const source = new SourceTranspiler(false).transpile(found, traversal).getCode();
      const idSource = UniqueIdentifier.get();
      const id = UniqueIdentifier.get();
      return new Chunk(`const ${this.syIndexBackup} = abap.builtin.sy.get().index.get();
const ${idSource} = new abap.types.Integer().set(${source}).get();
for (let ${id} = 0; ${id} < ${idSource}; ${id}++) {
abap.builtin.sy.get().index.set(${id} + 1);`);
    } else {
      const unique = UniqueIdentifier.get();
      return new Chunk(`const ${this.syIndexBackup} = abap.builtin.sy.get().index.get();
let ${unique} = 1;
while (true) {
abap.builtin.sy.get().index.set(${unique}++);`);
    }
  }

}
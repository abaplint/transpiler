import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import * as StatementTranspilers from "../src/statements";
import * as StructureTranspilers from "../src/structures";

/** the statements Traversal.traverseStatement() can be given: the children of the structures that have no transpiler
 * of their own, starting at the structures abaplint gives ABAP files. A structure with a transpiler of its own decides
 * itself what happens to its children. */
function reachableStatements(): Set<string> {
  const statements = new Set<string>();
  const seen = new Set<string>();
  const walk = (name: string) => {
    if (seen.has(name)) {
      return;
    }
    seen.add(name);
    const using: string[] = new (abaplint.Structures as any)[name]().getMatcher().getUsing();
    for (const used of using) {
      const [kind, child] = used.split("/");
      if (kind === "structure" && (StructureTranspilers as any)[child + "Transpiler"] === undefined) {
        walk(child);
      } else if (kind === "statement" && (abaplint.Statements as any)[child] !== undefined) {
        statements.add(child);
      }
    }
  };
  for (const root of [abaplint.Structures.Any, abaplint.Structures.ClassGlobal, abaplint.Structures.InterfaceGlobal]) {
    walk(root.name);
  }
  return statements;
}

describe("Statement coverage", () => {

  it("every statement the traversal can reach has a transpiler", () => {
    const missing = Array.from(reachableStatements())
      .filter(name => (StatementTranspilers as any)[name + "Transpiler"] === undefined)
      .sort();
    expect(missing).to.deep.equal([]);
  });

});

import * as abaplint from "@abaplint/core";
import {expect} from "chai";
import {Rearranger} from "../src/rearranger";

// returns the Source node
function run(abap: string): abaplint.Nodes.ExpressionNode {
  const file = new abaplint.MemoryFile("zrearrange.prog.abap", abap);
  const reg = new abaplint.Registry().addFiles([file]).parse();
  const obj = reg.getFirstObject() as abaplint.ABAPObject;
  const stru = obj.getMainABAPFile()?.getStructure();
  expect(stru).to.not.equal(undefined);
  const rearranged = new Rearranger().run(stru!);
  const source = rearranged?.findFirstStatement(abaplint.Statements.Move)?.findFirstExpression(abaplint.Expressions.Source);
  expect(source).to.not.equal(undefined);
  return source!;
}

describe("Rearranger", () => {

  it("test 1", async () => {
    const abap = `
      DATA int TYPE i.
      int = 5 - 1 + 1.`;
    const source = run(abap);
    expect(source).to.not.equal(undefined);
  });

});
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
  const rearranged = new Rearranger().run(obj.getType(), stru!);
  const source = rearranged?.findFirstStatement(abaplint.Statements.Move)?.findFirstExpression(abaplint.Expressions.Source);
  expect(source).to.not.equal(undefined);
  return source!;
}

function dump(node: abaplint.INode): string {
  const children = node.getChildren();
  if (children.length === 3) {
    let operator = "";
    switch (children[1].getFirstToken().getStr().toUpperCase()) {
      case "-":
        operator = "minus";
        break;
      case "+":
        operator = "plus";
        break;
      case "*":
        operator = "mult";
        break;
      case "BIT-AND":
        operator = "bitand";
        break;
      default:
        operator = "unknownOperator";
        break;
    }

    return operator + "(" + dump(children[0]) + ", " + dump(children[2]) + ")";
  } else if (children.length === 1) {
    return children[0].getFirstToken().getStr();
  } else {
    return "not-well-formed: " + children.map(c => c.getFirstToken().getStr()).join(" ");
  }
}

describe("The Rearranger of Nodes", () => {

  it("simple", async () => {
    const abap = `
      DATA int TYPE i.
      int = 5 - 1.`;
    const source = run(abap);
    const text = dump(source);
    expect(text).to.equal("minus(5, 1)");
  });

  it("test 1, evaluation must be left to right", async () => {
    const abap = `
      DATA int TYPE i.
      int = 5 - 1 + 1.`;
    const source = run(abap);
    const text = dump(source);
    expect(text).to.equal("plus(minus(5, 1), 1)");
  });

  it("test 2, evaluation must be left to right", async () => {
    const abap = `
      DATA int TYPE i.
      int = 5 - 1 + 1 + 4.`;
    const source = run(abap);
    const text = dump(source);
    expect(text).to.equal("plus(plus(minus(5, 1), 1), 4)");
  });

  it("test 3, multiplication", async () => {
    const abap = `
      DATA int TYPE i.
      int = 5 + 2 * 2.`;
    const source = run(abap);
    const text = dump(source);
    expect(text).to.equal("plus(5, mult(2, 2))");
  });

  it.only("test 4, BIT-AND and CONV", async () => {
    const abap = `
DATA binary_code TYPE x LENGTH 2.
DATA binary_zero TYPE x LENGTH 2.
DATA sdf TYPE x LENGTH 2.
sdf = binary_code BIT-AND CONV xstring( 16 ).`;
    const source = run(abap);
    const text = dump(source);
    expect(text).to.equal("bitand(binary_code, conv)");
  });

});
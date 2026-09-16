import {expect} from "chai";
import {ABAP} from "../src";

describe("Arithmetics", () => {

  it("Set initial value", () => {
    const abap = new ABAP();
    const foo = new abap.types.Integer();
    const bar = new abap.types.Integer();
    foo.set(bar);

    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(0);
  });


  // MOD follows the calculation type of its operands: with a float operand
  // the remainder is a float. It used to come back in an Integer, so
  // 2.75 MOD 1 was 1.
  it("MOD with a float operand answers a float", () => {
    const abap = new ABAP();
    const f = new abap.types.Float();
    f.set(2.75);
    const one = new abap.types.Integer();
    one.set(1);
    const r = abap.operators.mod(f as any, one);
    expect(r).to.be.instanceof(abap.types.Float);
    expect((r as any).getRaw()).to.equal(0.75);
    const i = new abap.types.Integer();
    i.set(7);
    const two = new abap.types.Integer();
    two.set(2);
    expect(abap.operators.mod(i, two).get()).to.equal(1);
  });
});

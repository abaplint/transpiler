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

  // A float exactly on a half is rounded away from zero when it is moved to
  // an integer, on both sides: 0.5 is 1 and -0.5 is -1. Math.round rounds a
  // half towards positive infinity, so -0.5 used to be 0.
  it("a negative half moved to an integer rounds away from zero", () => {
    const abap = new ABAP();
    const move = (v: number) => {
      const f = new abap.types.Float();
      f.set(v);
      const i = new abap.types.Integer();
      i.set(f);
      return i.get();
    };
    expect(move(0.5)).to.equal(1);
    expect(move(1.5)).to.equal(2);
    expect(move(-0.5)).to.equal(-1);
    expect(move(-1.5)).to.equal(-2);
    expect(move(-0.4)).to.equal(0);
    expect(move(2.4)).to.equal(2);
  });
});

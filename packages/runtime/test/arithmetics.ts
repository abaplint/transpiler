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

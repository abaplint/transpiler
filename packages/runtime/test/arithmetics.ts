import {expect} from "chai";
import {ABAP} from "../src/index.js";

describe("Arithmetics", () => {

  it("Set initial value", () => {
    const abap = new ABAP();
    const foo = new abap.types.Integer();
    const bar = new abap.types.Integer();
    foo.set(bar);

    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(0);
  });

  it("2 + 2", () => {
    const abap = new ABAP();
    const foo = new abap.types.Integer();
    foo.set(2);
    const bar = new abap.types.Integer();
    bar.set(2);
    const boo = new abap.types.Integer();
    boo.set(2);

    boo.set(abap.operators.add(foo, bar));

    expect(boo.get()).to.equal(4);
  });

});
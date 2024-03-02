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

});
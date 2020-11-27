import {expect} from "chai";
import {ABAP} from "../src";

describe("Define variables", () => {
  it("Define an integer", () => {
    const abap = new ABAP();
    const foo = new abap.types.Integer();
    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(0);
  });
});
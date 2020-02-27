import {expect} from "chai";
import * as abap from "../../src/runtime";

describe("Define variables", () => {
  it("Define an integer", () => {
    const foo = new abap.basictypes.I();
    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(0);
  });

  it("Define an integer with initial value", () => {
    const foo = new abap.basictypes.I(2);
    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(2);
  });
});
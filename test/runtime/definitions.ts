import {expect} from "chai";
import * as abap from "../../src/runtime";

describe("Define variables", () => {
  it("Define an integer", () => {
    let foo = new abap.basictypes.i();
    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(0);
  });

  it("Define an integer with initial value", () => {
    let foo = new abap.basictypes.i(2);
    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(2);
  });
});
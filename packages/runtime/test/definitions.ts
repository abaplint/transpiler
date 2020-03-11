import {expect} from "chai";
import * as abap from "../src";

describe("Define variables", () => {
  it("Define an integer", () => {
    const foo = new abap.types.Integer();
    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(0);
  });
});
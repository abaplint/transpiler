import {expect} from "chai";
import * as abap from "../src";

describe("Compare", () => {
  it("2 = 2", () => {
    const foo = new abap.types.Integer();
    foo.set(2);
    const bar = new abap.types.Integer();
    bar.set(2);
    const bool = abap.compare.eq(foo, bar);
    expect(bool).to.equal(true);
  });

  it("empty string", () => {
    const foo = new abap.types.String();
    const bool = abap.compare.eq(foo, "");
    expect(bool).to.equal(true);
  });
});
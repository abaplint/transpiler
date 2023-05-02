import {expect} from "chai";
import {ABAP} from "../src/index.js";

describe("Compare", () => {
  it("2 = 2", () => {
    const abap = new ABAP();
    const foo = new abap.types.Integer();
    foo.set(2);
    const bar = new abap.types.Integer();
    bar.set(2);
    const bool = abap.compare.eq(foo, bar);
    expect(bool).to.equal(true);
  });

  it("empty string", () => {
    const abap = new ABAP();
    const foo = new abap.types.String();
    const bool = abap.compare.eq(foo, "");
    expect(bool).to.equal(true);
  });
});
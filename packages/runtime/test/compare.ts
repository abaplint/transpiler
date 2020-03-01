import {expect} from "chai";
import * as abap from "../src";

describe("Compare", () => {
  it("2 = 2", () => {
    const foo = new abap.types.Integer({value: 2});
    const bar = new abap.types.Integer({value: 2});

    const bool = foo.eq(bar);

    expect(bool).to.equal(true);
  });

  it("empty string", () => {
    const foo = new abap.types.String();

    const bool = foo.eq('');

    expect(bool).to.equal(true);
  });
});
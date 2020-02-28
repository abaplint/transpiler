import {expect} from "chai";
import * as abap from "../../src/runtime";

describe("Compare", () => {
  it("2 = 2", () => {
    const foo = new abap.basictypes.Integer({value: 2});
    const bar = new abap.basictypes.Integer({value: 2});

    const bool = foo.equals(bar);

    expect(bool).to.equal(true);
  });
});
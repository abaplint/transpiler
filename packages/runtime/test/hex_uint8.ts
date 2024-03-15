import {expect} from "chai";
import {ABAP} from "../src";

describe("Hex UInt8", () => {

  it("Test", () => {
    const abap = new ABAP();
    const foo = new abap.types.HexUInt8({length: 2});
    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal("0000");
    foo.set("1234");
    expect(foo.get()).to.equal("1234");
  });

});
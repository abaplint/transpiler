import {expect} from "chai";
import {ABAP} from "../src/index.js";

describe("Console", () => {

  it("test 1", () => {
    const abap = new ABAP();
    const str = "bar";
    abap.statements.write(str);
    expect(abap.console.get()).to.equal(str);
    abap.console.clear();
    expect(abap.console.get()).to.equal("");
  });

});
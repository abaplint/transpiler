import {expect} from "chai";
import {ABAP, MemoryConsole} from "../src";

describe("Console", () => {

  it("test 1", () => {
    const abap = new ABAP(new MemoryConsole());
    const str = "bar";
    abap.statements.write(str);
    expect(abap.console.get()).to.equal(str);
    abap.console.clear();
    expect(abap.console.get()).to.equal("");
  });

});
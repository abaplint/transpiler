import {expect} from "chai";
import {ABAP} from "../../src";

describe("Statement WRITE", () => {

  it("Append an integer", () => {
    const abap = new ABAP();
    abap.statements.write("hello world");
    expect(abap.getConsole().get()).to.equal("hello world");
  });

});


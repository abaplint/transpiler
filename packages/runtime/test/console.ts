import {expect} from "chai";
import * as abap from "../src";

describe("Console", () => {

  it("test 1", () => {
    const str = "bar";
    abap.statements.write(str);
    expect(abap.Console.get()).to.equal(str);
    abap.Console.clear();
    expect(abap.Console.get()).to.equal("");
  });

});
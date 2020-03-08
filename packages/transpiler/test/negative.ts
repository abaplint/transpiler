import {expect} from "chai";
import {Transpiler} from "../src";

describe("Negative tests", () => {
  it("Unknown variable, throws error", () => {
    const abap = `WRITE foowrite.`;

    expect(() => new Transpiler().run(abap)).to.throw("not found");
  });
});
import {expect} from "chai";
import {Transpiler} from "../src";

describe("Validation", async () => {
  it("Unknown variable, throws error", () => {
    const abap = `WRITE foowrite.`;

    expect(() => new Transpiler().run(abap)).to.throw("not found");
  });

  it("Unknown type, throws error", () => {
    const abap = `DATA foo TYPE sdfsd.`;

    expect(() => new Transpiler().run(abap)).to.throw("not found");
  });
});
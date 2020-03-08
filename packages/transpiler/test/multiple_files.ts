import {expect} from "chai";
import {Transpiler} from "../src";

describe("Multiple files", () => {

  it("Two reports", async () => {
    const file1 = {filename: "zfoo1.prog.abap", contents: "WRITE '1'."};
    const file2 = {filename: "zfoo2.prog.abap", contents: "WRITE '2'."};

    const output = (await new Transpiler().runMulti([file1, file2])).js;

    expect(output.length).to.equal(2);
    expect(output[0].filename).to.equal("zfoo1.prog.js");
    expect(output[1].filename).to.equal("zfoo2.prog.js");
  });

});
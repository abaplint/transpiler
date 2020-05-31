import {expect} from "chai";
import {Transpiler} from "../src";

describe("Files", () => {

  it("Two reports", async () => {
    const file1 = {filename: "zfoo1.prog.abap", contents: "WRITE '1'."};
    const file2 = {filename: "zfoo2.prog.abap", contents: "WRITE '2'."};

    const output = (await new Transpiler().run([file1, file2]));

    expect(output.length).to.equal(2);
    expect(output[0].js.filename).to.equal("zfoo1.prog.js");
    expect(output[1].js.filename).to.equal("zfoo2.prog.js");
  });

  it("Full path file name", async () => {
    const filename = "C:\\Users\\foobar\\git\\transpiler\\packages\\abap-loader\\build\\test\\zprogram.prog.abap";
    const file1 = {filename, contents: "WRITE '1'."};

    const output = (await new Transpiler().run([file1]));

    expect(output.length).to.equal(1);
    expect(output[0].js.filename).to.contain("zprogram.prog.js");
  });

  it("Global Class", async () => {
    const filename = "zcl_index.clas.abap";
    const contents = `
CLASS zcl_index DEFINITION PUBLIC.
ENDCLASS.
CLASS zcl_index IMPLEMENTATION.
ENDCLASS.
`;
    const file1 = {filename, contents};

    const output = (await new Transpiler().run([file1]));

    expect(output.length).to.equal(1);
    expect(output[0].js.contents).to.match(/^export /i);
  });

});
import * as path from "path";
import * as fs from "fs";
import * as childProcess from "child_process";
import {expect} from "chai";
import {IFile, ITranspilerOptions, Transpiler} from "../packages/transpiler/src/";

describe("Testing Unit Testing", () => {
  const base: string = path.join(__dirname, "..", "..", "unit-test/");
  let name: string | undefined = "";
  let outputFolder: string = "";

  beforeEach(function() {
    if (fs.existsSync(base) === false) {
      fs.mkdirSync(base);
    }
    name = this.currentTest?.title;
    outputFolder = base + this.currentTest?.title;
    if (fs.existsSync(outputFolder) === false) {
      fs.mkdirSync(outputFolder);
    }

    for (const file of fs.readdirSync(outputFolder)) {
      fs.unlinkSync(outputFolder + path.sep + file);
    }
  });

  async function dumpNrun(files: IFile[]): Promise<string> {
    const config: ITranspilerOptions = {
      addCommonJS: true,
    };
    const output = await new Transpiler(config).run(files);

    for (const o of output.objects) {
      fs.writeFileSync(outputFolder + path.sep + o.js.filename, o.js.contents);
    }
    // hack
    output.unitTest = output.unitTest.replace(`require("@abaplint/runtime");`,
                                              `require("../../packages/runtime/build/src/index.js");`);
    const indexName = outputFolder + path.sep + "index.js";
    fs.writeFileSync(indexName, output.unitTest);
    const buf = childProcess.execSync("node unit-test/" + name + "/index.js");
    return buf.toString();
  }

  it("test-1", async () => {
    const clas = `
    CLASS zcl_client DEFINITION PUBLIC.
    ENDCLASS.
    CLASS zcl_client IMPLEMENTATION.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        WRITE 'hello world'.
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons).to.include("hello world");
  });

});
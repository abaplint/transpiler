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
    output.unitTest = output.unitTest.replace(`import runtime from "@abaplint/runtime";`,
                                              `import runtime from "../../packages/runtime/build/src/index.js";`);
    const indexName = outputFolder + path.sep + "index.mjs";
    fs.writeFileSync(indexName, output.unitTest);
    const buf = childProcess.execSync("node unit-test/" + name + "/index.mjs");
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
    expect(cons.split("\n")[1]).to.equal("hello world");
  });

  it("test-2", async () => {
// instantiating class + using abap.types.Integer
    const clas = `
    CLASS zcl_client DEFINITION PUBLIC.
      PUBLIC SECTION.
        METHODS method.
    ENDCLASS.
    CLASS zcl_client IMPLEMENTATION.
      METHOD method.
        DATA int TYPE i.
        int = 2.
        WRITE: / 'hello from method', int.
      ENDMETHOD.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA ref TYPE REF TO zcl_client.
        CREATE OBJECT ref.
        ref->method( ).
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("hello from method2");
  });

  it("test-3", async () => {
// class constructor
    const clas = `
    CLASS zcl_client DEFINITION PUBLIC.
      PUBLIC SECTION.
        CLASS-DATA gv_int TYPE i.
        CLASS-METHODS class_constructor.
    ENDCLASS.
    CLASS zcl_client IMPLEMENTATION.
      METHOD class_constructor.
        gv_int = 42.
      ENDMETHOD.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        WRITE / zcl_client=>gv_int.
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("42");
  });

  it("test-4", async () => {
// create object, reference to itself
    const clas = `
    CLASS zcl_client DEFINITION PUBLIC.
      PUBLIC SECTION.
        METHODS method.
        METHODS write.
    ENDCLASS.
    CLASS zcl_client IMPLEMENTATION.
      METHOD write.
        WRITE / 'moo'.
      ENDMETHOD.
      METHOD method.
        DATA ref TYPE REF TO zcl_client.
        CREATE OBJECT ref.
        ref->write( ).
      ENDMETHOD.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA ref TYPE REF TO zcl_client.
        CREATE OBJECT ref.
        ref->method( ).
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("moo");
  });

});
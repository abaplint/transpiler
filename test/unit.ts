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

    for (const f of files) {
      fs.writeFileSync(outputFolder + path.sep + f.filename, f.contents);
    }

    const output = await new Transpiler(config).run(files);

    for (const o of output.objects) {
      let contents = o.js.contents;
      contents = contents + `\n//# sourceMappingURL=` + o.sourceMap.filename;
      fs.writeFileSync(outputFolder + path.sep + o.sourceMap.filename, o.sourceMap.contents);
      fs.writeFileSync(outputFolder + path.sep + o.js.filename, contents);
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

  it("test-5", async () => {
// cycles, typedescr instantiates structdescr, structdescr implements typedescr
    const clas = `
    CLASS cl_abap_typedescr DEFINITION PUBLIC.
      PUBLIC SECTION.
        METHODS method.
    ENDCLASS.
    CLASS cl_abap_typedescr IMPLEMENTATION.
      METHOD method.
        DATA ref TYPE REF TO cl_abap_structdescr.
        CREATE OBJECT ref.
        ref->moo( ).
      ENDMETHOD.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA ref TYPE REF TO cl_abap_typedescr.
        CREATE OBJECT ref.
        ref->method( ).
      ENDMETHOD.
    ENDCLASS.`;
    const stru = `
    CLASS cl_abap_structdescr DEFINITION PUBLIC INHERITING FROM cl_abap_typedescr.
      PUBLIC SECTION.
        METHODS moo.
    ENDCLASS.
    CLASS cl_abap_structdescr IMPLEMENTATION.
      METHOD moo.
        WRITE / 'done'.
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "cl_abap_typedescr.clas.abap", contents: clas},
      {filename: "cl_abap_typedescr.clas.testclasses.abap", contents: tests},
      {filename: "cl_abap_structdescr.clas.abap", contents: stru},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("done");
  });

  it("test-6", async () => {
// static method call
    const clas = `
    CLASS zcl_client DEFINITION PUBLIC.
      PUBLIC SECTION.
        CLASS-METHODS method.
    ENDCLASS.
    CLASS zcl_client IMPLEMENTATION.
      METHOD method.
        WRITE / 'moo'.
      ENDMETHOD.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        zcl_client=>method( ).
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("moo");
  });

  it("test-7", async () => {
// write constant from interface
    const intf = `
    INTERFACE if_bar PUBLIC.
      CONSTANTS value TYPE i VALUE 2.
    ENDINTERFACE.`;
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
        WRITE if_bar=>value.
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "if_bar.intf.abap", contents: intf},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("2");
  });

  it("test-8", async () => {
// CLAS locals includes
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
        lcl_mapping_camel=>run( ).
      ENDMETHOD.
    ENDCLASS.`;
    const def = `
    CLASS lcl_mapping_camel DEFINITION.
      PUBLIC SECTION.
        CLASS-METHODS run.
    ENDCLASS.`;
    const imp = `
    CLASS lcl_mapping_camel IMPLEMENTATION.
      METHOD run.
        WRITE 'from impl'.
      ENDMETHOD.
    ENDCLASS.`;
    const cxroot = `
    CLASS cx_root DEFINITION PUBLIC.
    ENDCLASS.
    CLASS cx_root IMPLEMENTATION.
    ENDCLASS.`;
    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.locals_def.abap", contents: def},
      {filename: "zcl_client.clas.locals_imp.abap", contents: imp},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("from impl");
  });

  it("test-9", async () => {
// newline constant referenced
    const charutils = `CLASS cl_abap_char_utilities DEFINITION PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      newline TYPE c LENGTH 1 VALUE '_'.
    CLASS-METHODS:
      class_constructor.
ENDCLASS.
CLASS cl_abap_char_utilities IMPLEMENTATION.
  METHOD class_constructor.
    WRITE '@KERNEL cl_abap_char_utilities.newline.set("\\n");'.
  ENDMETHOD.
ENDCLASS.`;
    const intf = `
    INTERFACE zif_abapgit_definitions PUBLIC.
      CONSTANTS c_newline TYPE c LENGTH 1 VALUE cl_abap_char_utilities=>newline ##NO_TEXT.
    ENDINTERFACE.`;
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
        WRITE zif_abapgit_definitions=>c_newline.
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "cl_abap_char_utilities.clas.abap", contents: charutils},
      {filename: "zif_abapgit_definitions.intf.abap", contents: intf},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n").length).to.equal(4);
  });

  it("test-10", async () => {
// CLAS locals includes, testclass class inheriting from locals class
    const clas = `
    CLASS zcl_client DEFINITION PUBLIC.
    ENDCLASS.
    CLASS zcl_client IMPLEMENTATION.
    ENDCLASS.`;
    const tests = `
    CLASS lcl_buffer DEFINITION INHERITING FROM lcl_mapping_camel.
    ENDCLASS.
    CLASS lcl_buffer IMPLEMENTATION.
    ENDCLASS.

    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        lcl_mapping_camel=>run( ).
      ENDMETHOD.
    ENDCLASS.`;
    const def = `
    CLASS lcl_mapping_camel DEFINITION.
      PUBLIC SECTION.
        CLASS-METHODS run.
    ENDCLASS.`;
    const imp = `
    CLASS lcl_mapping_camel IMPLEMENTATION.
      METHOD run.
        WRITE 'from impl'.
      ENDMETHOD.
    ENDCLASS.`;
    const cxroot = `
    CLASS cx_root DEFINITION PUBLIC.
    ENDCLASS.
    CLASS cx_root IMPLEMENTATION.
    ENDCLASS.`;
    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.locals_def.abap", contents: def},
      {filename: "zcl_client.clas.locals_imp.abap", contents: imp},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("from impl");
  });

  it("test-11", async () => {
// TYPE GROUP
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
        DATA e TYPE abap_encoding.
        e = 'UTF'.
        WRITE e.
      ENDMETHOD.
    ENDCLASS.`;
    const type = `TYPE-POOL abap.

TYPES abap_encoding TYPE c LENGTH 20.`;
    const files = [
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "abap.type.abap", contents: type},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("UTF");
  });

  it("test-12", async () => {
// local testclass inheriting from global class
    const clas = `
    CLASS zcl_client DEFINITION PUBLIC.
    ENDCLASS.
    CLASS zcl_client IMPLEMENTATION.
    ENDCLASS.`;
    const tests = `
CLASS ltcl_xml_concrete DEFINITION FOR TESTING
    FINAL
    INHERITING FROM zcl_client.
ENDCLASS.

CLASS ltcl_xml_concrete IMPLEMENTATION.
ENDCLASS.`;
    const files = [
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    await dumpNrun(files);
  });

  it("test-13", async () => {
// Catch
    const clas = `
    CLASS zcx_error DEFINITION PUBLIC INHERITING FROM cx_root.
    ENDCLASS.
    CLASS zcx_error IMPLEMENTATION.
    ENDCLASS.`;
    const clas2 = `
    CLASS zcx_something DEFINITION PUBLIC INHERITING FROM cx_root.
    ENDCLASS.
    CLASS zcx_something IMPLEMENTATION.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        TRY.
          RAISE EXCEPTION TYPE zcx_error.
        CATCH zcx_something.
          WRITE 'blah'.
        CATCH zcx_error.
          WRITE 'hello'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;
    const cxroot = `
    CLASS cx_root DEFINITION PUBLIC.
    ENDCLASS.
    CLASS cx_root IMPLEMENTATION.
    ENDCLASS.`;
    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "zcx_something.clas.abap", contents: clas2},
      {filename: "zcx_error.clas.abap", contents: clas},
      {filename: "zcx_error.clas.testclasses.abap", contents: tests},
    ];
    const console = await dumpNrun(files);
    expect(console.split("\n")[1]).to.equal("hello");
  });

  it("test-14", async () => {
// global test class without test methods
    const clas = `CLASS zcl_abapgit_persist_injector DEFINITION PUBLIC CREATE PRIVATE FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS set_repo.
ENDCLASS.
CLASS ZCL_ABAPGIT_PERSIST_INJECTOR IMPLEMENTATION.
  METHOD set_repo.
    WRITE 'hello'.
  ENDMETHOD.
ENDCLASS.`;
    const files = [
      {filename: "zcl_abapgit_persist_injector.clas.abap", contents: clas},
    ];
    await dumpNrun(files);
  });

  it("test-15", async () => {
// dyanmic instantiation of class by name
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
        DATA li_bar TYPE REF TO object.
        TRY.
            CREATE OBJECT li_bar TYPE ('ZCL_SDFSD').
          CATCH cx_sy_create_object_error.
            WRITE 'not found'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;
    const cxroot = `
    CLASS cx_root DEFINITION PUBLIC.
    ENDCLASS.
    CLASS cx_root IMPLEMENTATION.
    ENDCLASS.`;
    const cxcreate = `
    CLASS cx_sy_create_object_error DEFINITION PUBLIC INHERITING FROM cx_root.
    ENDCLASS.
    CLASS cx_sy_create_object_error IMPLEMENTATION.
    ENDCLASS.`;
    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_create_object_error.clas.abap", contents: cxcreate},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("not found");
  });

});
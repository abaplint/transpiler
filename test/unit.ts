import * as path from "path";
import * as fs from "fs";
import * as childProcess from "child_process";
import {expect} from "chai";
import {Transpiler} from "../packages/transpiler/src/";
import {IFile, ITranspilerOptions} from "../packages/transpiler/src/types";
import * as abaplint from "@abaplint/core";

const t000 = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>T000</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <DDTEXT>T000</DDTEXT>
    <CONTFLAG>A</CONTFLAG>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD09L>
    <TABNAME>T000</TABNAME>
    <AS4LOCAL>A</AS4LOCAL>
    <TABKAT>0</TABKAT>
    <TABART>APPL0</TABART>
    <BUFALLOW>N</BUFALLOW>
   </DD09L>
   <DD03P_TABLE>
    <DD03P>
     <TABNAME>T000</TABNAME>
     <FIELDNAME>MANDT</FIELDNAME>
     <DDLANGUAGE>E</DDLANGUAGE>
     <POSITION>0001</POSITION>
     <KEYFLAG>X</KEYFLAG>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000006</INTLEN>
     <NOTNULL>X</NOTNULL>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000003</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
    <DD03P>
     <TABNAME>T000</TABNAME>
     <FIELDNAME>CCCATEGORY</FIELDNAME>
     <DDLANGUAGE>E</DDLANGUAGE>
     <POSITION>0002</POSITION>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000002</INTLEN>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000001</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
    <DD03P>
     <TABNAME>T000</TABNAME>
     <FIELDNAME>CCNOCLIIND</FIELDNAME>
     <DDLANGUAGE>E</DDLANGUAGE>
     <POSITION>0003</POSITION>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000002</INTLEN>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000001</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

const zopentest = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZOPENTEST</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <DDTEXT>ZOPENTEST</DDTEXT>
    <MAINFLAG>X</MAINFLAG>
    <CONTFLAG>A</CONTFLAG>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD09L>
    <TABNAME>ZOPENTEST</TABNAME>
    <AS4LOCAL>A</AS4LOCAL>
    <TABKAT>0</TABKAT>
    <TABART>APPL1</TABART>
    <BUFALLOW>N</BUFALLOW>
   </DD09L>
   <DD03P_TABLE>
    <DD03P>
     <FIELDNAME>KEYFIELD</FIELDNAME>
     <KEYFLAG>X</KEYFLAG>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000008</INTLEN>
     <NOTNULL>X</NOTNULL>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000004</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
    <DD03P>
     <FIELDNAME>VALUEFIELD</FIELDNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000020</INTLEN>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000010</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

const cxroot = `
CLASS cx_root DEFINITION PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.`;

const cxcreate = `
CLASS cx_sy_range_out_of_bounds DEFINITION PUBLIC INHERITING FROM cx_root.
ENDCLASS.
CLASS cx_sy_range_out_of_bounds IMPLEMENTATION.
ENDCLASS.`;

const cxmovecast = `
CLASS cx_sy_move_cast_error DEFINITION PUBLIC INHERITING FROM cx_root.
ENDCLASS.
CLASS cx_sy_move_cast_error IMPLEMENTATION.
ENDCLASS.`;

const ezlock = `
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_ENQU" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD25V>
    <VIEWNAME>EZLOCK</VIEWNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <AGGTYPE>E</AGGTYPE>
    <ROOTTAB>ZLOCK</ROOTTAB>
    <DDTEXT>Lock</DDTEXT>
   </DD25V>
   <DD26E_TABLE>
    <DD26E>
     <VIEWNAME>EZLOCK</VIEWNAME>
     <TABNAME>ZLOCK</TABNAME>
     <TABPOS>0001</TABPOS>
     <FORTABNAME>ZLOCK</FORTABNAME>
     <ENQMODE>E</ENQMODE>
    </DD26E>
   </DD26E_TABLE>
   <DD27P_TABLE>
    <DD27P>
     <VIEWNAME>EZLOCK</VIEWNAME>
     <OBJPOS>0001</OBJPOS>
     <VIEWFIELD>FIELD</VIEWFIELD>
     <TABNAME>ZLOCK</TABNAME>
     <FIELDNAME>FIELD</FIELDNAME>
     <KEYFLAG>X</KEYFLAG>
     <ENQMODE>E</ENQMODE>
    </DD27P>
   </DD27P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

const zlock = `
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZLOCK</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>INTTAB</TABCLASS>
    <DDTEXT>Lock</DDTEXT>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD03P_TABLE>
    <DD03P>
     <FIELDNAME>FIELD</FIELDNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000008</INTLEN>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000004</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

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

  async function dumpNrun(files: IFile[], database = true): Promise<string> {
    const SETUP_NAME = "mysetup.mjs";
    const config: ITranspilerOptions = {
      addCommonJS: true,
      extraSetup: "./" + SETUP_NAME,
    };

    for (const f of files) {
      fs.writeFileSync(outputFolder + path.sep + f.filename, f.contents);
    }

    let setupLogic = `import {SQLiteDatabaseClient} from "../../packages/database-sqlite/build/index.js";

export async function setup(abap, schemas, insert) {
  abap.context.databaseConnections["DEFAULT"] = new SQLiteDatabaseClient();
  await abap.context.databaseConnections["DEFAULT"].connect();
  await abap.context.databaseConnections["DEFAULT"].execute(schemas.sqlite);
  await abap.context.databaseConnections["DEFAULT"].execute(insert);
}`;
    if (database === false) {
      setupLogic = `export async function setup(abap, schemas, insert) {
  return;
}`;
    }
    fs.writeFileSync(outputFolder + path.sep + SETUP_NAME, setupLogic);

    const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
    const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
    const output = await new Transpiler(config).run(reg);

    for (const o of output.objects) {
      let contents = o.chunk.getCode();
      if (o.object.type.toUpperCase() !== "TABL"
          && o.object.type.toUpperCase() !== "DTEL"
          && o.object.type.toUpperCase() !== "ENQU"
          && o.object.type.toUpperCase() !== "MSAG"
          && o.object.type.toUpperCase() !== "TTYP") {
        const name = o.filename + ".map";
        contents = contents + `\n//# sourceMappingURL=` + name;
        fs.writeFileSync(outputFolder + path.sep + name, o.chunk.getMap(o.filename));
      }
      fs.writeFileSync(outputFolder + path.sep + o.filename, contents);
    }

    // hacks
    output.unitTestScript = output.unitTestScript.replace(
      `import runtime from "@abaplint/runtime";`,
      `import runtime from "../../packages/runtime/build/src/index.js";`);
    fs.writeFileSync(outputFolder + path.sep + "index.mjs", output.unitTestScript);
    output.unitTestScriptOpen = output.unitTestScriptOpen.replace(
      `import runtime from "@abaplint/runtime";`,
      `import runtime from "../../packages/runtime/build/src/index.js";`);
    fs.writeFileSync(outputFolder + path.sep + "index_open.mjs", output.unitTestScriptOpen);
    output.initializationScript = output.initializationScript.replace(
      `import runtime from "@abaplint/runtime";`,
      `import runtime from "../../packages/runtime/build/src/index.js";`);
    fs.writeFileSync(outputFolder + path.sep + "init.mjs", output.initializationScript);

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
    expect(cons.split("\n").length).to.equal(3);
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
    expect(cons.split("\n")[1].trimEnd()).to.equal("UTF");
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
        METHODS test02 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.

      METHOD test01.
        TRY.
          RAISE EXCEPTION TYPE zcx_error.
        CATCH zcx_something.
          WRITE 'blah1'.
        CATCH zcx_error.
          WRITE 'hello'.
        ENDTRY.
      ENDMETHOD.

      METHOD test02.
        DATA temp2 TYPE REF TO zcx_error.
        TRY.
          CREATE OBJECT temp2.
          RAISE EXCEPTION temp2.
        CATCH zcx_something.
          WRITE 'blah2'.
        CATCH zcx_error.
          WRITE 'world'.
        ENDTRY.
      ENDMETHOD.

    ENDCLASS.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "zcx_something.clas.abap", contents: clas2},
      {filename: "zcx_error.clas.abap", contents: clas},
      {filename: "zcx_error.clas.testclasses.abap", contents: tests},
    ];
    const console = await dumpNrun(files);
    expect(console).to.include("hello");
    expect(console).to.include("world");
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

  it("test-16", async () => {
// used for manual testing of source maps
    const clas = `CLASS zcl_sourcemaptest DEFINITION PUBLIC CREATE PRIVATE FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS set_repo.
ENDCLASS.
CLASS zcl_sourcemaptest IMPLEMENTATION.
  METHOD set_repo.
    DATA foo TYPE i.
    DATA bar TYPE i.
    DATA list TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    APPEND foo TO list.
    WRITE 'hello'.
    IF foo = bar.
      WRITE 'moo'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.`;
    const files = [
      {filename: "zcl_sourcemaptest.clas.abap", contents: clas},
    ];
    await dumpNrun(files);
  });

  it("test-17", async () => {
// CALL TRANSFORMATION, check dummy implementation runs
    const clas = `CLASS kernel_call_transformation DEFINITION PUBLIC.
* handling of ABAP statement CALL TRANSFORMATION
  PUBLIC SECTION.
    CLASS-METHODS call IMPORTING input TYPE any.
ENDCLASS.

CLASS kernel_call_transformation IMPLEMENTATION.
  METHOD call.
    WRITE / 'itWorks'.
  ENDMETHOD.
ENDCLASS.`;

    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA lv_xml TYPE string.
        DATA: BEGIN OF ls_structure,
                field TYPE i,
              END OF ls_structure.
        CALL TRANSFORMATION id
          SOURCE XML lv_xml
          RESULT data = ls_structure.
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "kernel_call_transformation.clas.abap", contents: clas},
      {filename: "kernel_call_transformation.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("itWorks");
  });

  it("test-18", async () => {
// check private SETUP method in test superclass is called
    const clas = `CLASS zcl_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
ENDCLASS.
CLASS ZCL_TEST IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_base DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT ABSTRACT.
  PROTECTED SECTION.
    DATA val TYPE i.
  PRIVATE SECTION.
    METHODS setup.
ENDCLASS.

CLASS ltcl_base IMPLEMENTATION.
  METHOD setup.
    val = 2.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS INHERITING FROM ltcl_base.
  PRIVATE SECTION.
    METHODS sdfsd FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD sdfsd.
    ASSERT val = 2.
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "zcl_test.clas.abap", contents: clas},
      {filename: "zcl_test.clas.testclasses.abap", contents: tests},
    ];
    await dumpNrun(files);
  });

  it("test-19", async () => {
// structured default method importing constant from interface
    const intf = `INTERFACE zif_client PUBLIC.
  TYPES ty_visit_type TYPE i.

  CONSTANTS:
    BEGIN OF visit_type,
      value TYPE ty_visit_type VALUE 0,
      open  TYPE ty_visit_type VALUE 123,
    END OF visit_type.

  METHODS keep_node
    IMPORTING
      iv_visit TYPE ty_visit_type DEFAULT visit_type-open.
ENDINTERFACE.`;

    const clas = `CLASS zcl_client DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_client.
ENDCLASS.
CLASS zcl_client IMPLEMENTATION.
  METHOD zif_client~keep_node.
    WRITE iv_visit.
  ENDMETHOD.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS sdfsd FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD sdfsd.
    DATA foo TYPE REF TO zcl_client.
    CREATE OBJECT foo.
    foo->zif_client~keep_node( ).
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "zif_client.intf.abap", contents: intf},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("123");
  });

  it("test-20", async () => {
// data from interface
    const intf = `INTERFACE zif_client PUBLIC.
  DATA foo TYPE i.
ENDINTERFACE.`;

    const clas = `CLASS zcl_client DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_client.
    METHODS constructor.
ENDCLASS.

CLASS ZCL_CLIENT IMPLEMENTATION.
  METHOD constructor.
    zif_client~foo = 2221.
  ENDMETHOD.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS sdfsd FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD sdfsd.
    DATA foo TYPE REF TO zcl_client.
    CREATE OBJECT foo.
    WRITE foo->zif_client~foo.
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "zif_client.intf.abap", contents: intf},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("2221");
  });

  it("test-21", async () => {
// select from non-existing database table, should throw exception

    const cxroot = `
CLASS cx_root DEFINITION PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.`;

// "FROM cx_root" is not correct, but ok for the testcase
    const cx = `CLASS cx_sy_dynamic_osql_semantics DEFINITION PUBLIC INHERITING FROM cx_root.
ENDCLASS.
CLASS cx_sy_dynamic_osql_semantics IMPLEMENTATION.
ENDCLASS.`;

    const clas = `CLASS zcl_select_nono DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_select_nono IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS select FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD select.
    FIELD-SYMBOLS <fs> TYPE STANDARD TABLE.
    TRY.
      SELECT * FROM ('NONO') INTO TABLE <fs>.
      WRITE / 'fail'.
    CATCH cx_sy_dynamic_osql_semantics.
      WRITE / 'ok'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "t000.tabl.xml", contents: t000}, // one database table is required or database does not startup
      {filename: "cx_sy_dynamic_osql_semantics.clas.abap", contents: cx},
      {filename: "zcl_select_nono.clas.abap", contents: clas},
      {filename: "zcl_select_nono.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("ok");
  });

  it("test-22", async () => {
// dynamic select from existing table

    const clas = `CLASS zcl_select_t000 DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_select_t000 IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS select FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD select.
    DATA mv_table TYPE string VALUE 'T000'.
    DATA lt_tab TYPE STANDARD TABLE OF t000 WITH DEFAULT KEY.
    DATA ls_row LIKE LINE OF lt_tab.
    SELECT * FROM (mv_table) INTO TABLE lt_tab.
    ASSERT sy-subrc = 0.
    ASSERT lines( lt_tab ) = 1.
    READ TABLE lt_tab INDEX 1 INTO ls_row.
    ASSERT ls_row-mandt = sy-mandt.
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "t000.tabl.xml", contents: t000}, // one database table is required or database does not startup
      {filename: "zcl_select_t000.clas.abap", contents: clas},
      {filename: "zcl_select_t000.clas.testclasses.abap", contents: tests},
    ];
    await dumpNrun(files);
  });

  it("test-23", async () => {
// static select into field symbol

    const clas = `CLASS zcl_select_t000 DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_select_t000 IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS select FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD select.
    DATA tab TYPE STANDARD TABLE OF t000 WITH DEFAULT KEY.
    FIELD-SYMBOLS <fs> TYPE ANY TABLE.
    ASSIGN tab TO <fs>.
    SELECT * FROM t000 INTO TABLE <fs>.
    ASSERT lines( <fs> ) = 1.
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "t000.tabl.xml", contents: t000}, // one database table is required or database does not startup
      {filename: "zcl_select_t000.clas.abap", contents: clas},
      {filename: "zcl_select_t000.clas.testclasses.abap", contents: tests},
    ];
    await dumpNrun(files);
  });

  it("test-24", async () => {
// dynamic CREATE DATA

    const clas = `CLASS zcl_select_t000 DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_select_t000 IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS select FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD select.
    DATA dref TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE STANDARD TABLE.
    CREATE DATA dref TYPE STANDARD TABLE OF ('T000') WITH DEFAULT KEY.
    ASSIGN dref->* TO <fs>.
    APPEND INITIAL LINE TO <fs>.
    WRITE lines( <fs> ).
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "t000.tabl.xml", contents: t000}, // one database table is required or database does not startup
      {filename: "zcl_select_t000.clas.abap", contents: clas},
      {filename: "zcl_select_t000.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("1");
  });

  it("test-25", async () => {
// INSERT into database

    const clas = `CLASS zcl_insertdb DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_insertdb IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS insert FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD insert.

    DATA ls_row TYPE zopentest.
    SELECT SINGLE * FROM zopentest INTO ls_row.
    ASSERT sy-subrc <> 0.

    ls_row-keyfield = 'hihi'.
    ls_row-valuefield = 'world'.
    INSERT INTO zopentest VALUES ls_row.
    ASSERT sy-subrc = 0.

    INSERT INTO zopentest VALUES ls_row.
    ASSERT sy-subrc <> 0.

    SELECT SINGLE * FROM zopentest INTO ls_row.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "zopentest.tabl.xml", contents: zopentest},
      {filename: "zcl_insertdb.clas.abap", contents: clas},
      {filename: "zcl_insertdb.clas.testclasses.abap", contents: tests},
    ];
    await dumpNrun(files);
  });

  it("test-26", async () => {
// should raise cx_sy_conversion_no_number

    const clas = `CLASS zcl_conv DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_conv IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test.
    DATA int TYPE i.
    DATA str TYPE string.
    str = 'abc'.
    TRY.
        int = str.
      CATCH cx_sy_conversion_no_number.
        WRITE 'expected'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.`;

    const cxconv = `
    CLASS cx_sy_conversion_no_number DEFINITION PUBLIC INHERITING FROM cx_root.
    ENDCLASS.
    CLASS cx_sy_conversion_no_number IMPLEMENTATION.
    ENDCLASS.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_conversion_no_number.clas.abap", contents: cxconv},
      {filename: "zcl_conv.clas.abap", contents: clas},
      {filename: "zcl_conv.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-27", async () => {
// should raise cx_sy_dyn_call_illegal_method

    const clas = `CLASS zcl_call DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_call IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test.
    TRY.
        CALL METHOD zcl_call=>('NOT_FOUND').
        ASSERT 1 = 2.
      CATCH cx_sy_dyn_call_illegal_method.
        WRITE 'expected'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.`;



    const cxconv = `
    CLASS cx_sy_dyn_call_illegal_method DEFINITION PUBLIC INHERITING FROM cx_root.
    ENDCLASS.
    CLASS cx_sy_dyn_call_illegal_method IMPLEMENTATION.
    ENDCLASS.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_dyn_call_illegal_method.clas.abap", contents: cxconv},
      {filename: "zcl_call.clas.abap", contents: clas},
      {filename: "zcl_call.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-28", async () => {
// test value from exception

    const clas = `CLASS zcx_parameter_test DEFINITION PUBLIC INHERITING FROM cx_root CREATE PUBLIC.
  PUBLIC SECTION.
    DATA hello TYPE i .
    METHODS constructor
      IMPORTING
        hello    TYPE i OPTIONAL .
ENDCLASS.

CLASS zcx_parameter_test IMPLEMENTATION.
  METHOD constructor.
    me->hello = hello.
  ENDMETHOD.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.
  PRIVATE SECTION.
    METHODS sdfsd FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD sdfsd.
    DATA lcx TYPE REF TO zcx_parameter_test.
    TRY.
        RAISE EXCEPTION TYPE zcx_parameter_test
          EXPORTING
            hello = 2.
      CATCH zcx_parameter_test INTO lcx.
        WRITE lcx->hello.
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
      {filename: "zcx_parameter_test.clas.abap", contents: clas},
      {filename: "zcx_parameter_test.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("2");
  });

  it("test-29", async () => {
// throw cx_sy_range_out_of_bounds, character field
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
        DATA input TYPE c LENGTH 2.
        DATA letter TYPE c LENGTH 1.
        DATA offset TYPE i.
        offset = 3.
        TRY.
            letter = input+offset(1).
            WRITE 'bad bot'.
          CATCH cx_sy_range_out_of_bounds.
            WRITE 'expected'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_range_out_of_bounds.clas.abap", contents: cxcreate},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-30", async () => {
// throw cx_sy_range_out_of_bounds, string
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
        DATA input TYPE string.
        DATA letter TYPE c LENGTH 1.
        TRY.
            letter = input+10(1).
            WRITE 'bad bot'.
          CATCH cx_sy_range_out_of_bounds.
            WRITE 'expected'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;


    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_range_out_of_bounds.clas.abap", contents: cxcreate},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-31", async () => {
// substring() should throw cx_sy_range_out_of_bounds
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
        DATA str TYPE string.
        TRY.
            WRITE substring( val = str off = 5 ).
          CATCH cx_sy_range_out_of_bounds.
            WRITE 'expected'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;


    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_range_out_of_bounds.clas.abap", contents: cxcreate},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-32", async () => {
// replace() should throw cx_sy_range_out_of_bounds
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
        DATA str TYPE string.
        TRY.
            WRITE replace( val = str off = 5 with = 'sdf' len = 2 ).
          CATCH cx_sy_range_out_of_bounds.
            WRITE 'expected'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;


    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_range_out_of_bounds.clas.abap", contents: cxcreate},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-33", async () => {
// insert() should throw cx_sy_range_out_of_bounds
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
        DATA str TYPE string.
        TRY.
            WRITE insert( val = str off = 5 sub = 'sdf' ).
          CATCH cx_sy_range_out_of_bounds.
            WRITE 'expected'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;


    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_range_out_of_bounds.clas.abap", contents: cxcreate},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-34", async () => {
// substring() negative offset should throw cx_sy_range_out_of_bounds
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
        DATA str TYPE string.
        TRY.
            WRITE substring( val = |dsfs| off = -1 len = 1 ).
          CATCH cx_sy_range_out_of_bounds.
            WRITE 'expected'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_range_out_of_bounds.clas.abap", contents: cxcreate},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-35", async () => {
// cast should throw cx_sy_move_cast_error
    const bar = `
    CLASS zcl_bar DEFINITION PUBLIC.
    ENDCLASS.
    CLASS zcl_bar IMPLEMENTATION.
    ENDCLASS.`;

    const clas = `
    CLASS zcl_foo DEFINITION PUBLIC.
    ENDCLASS.
    CLASS zcl_foo IMPLEMENTATION.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA foo TYPE REF TO object.
        DATA bar TYPE REF TO zcl_bar.
        CREATE OBJECT foo TYPE zcl_foo.
        TRY.
            bar ?= foo.
          CATCH cx_sy_move_cast_error.
            WRITE 'expected'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_move_cast_error.clas.abap", contents: cxmovecast},
      {filename: "zcl_bar.clas.abap", contents: bar},
      {filename: "zcl_foo.clas.abap", contents: clas},
      {filename: "zcl_foo.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-36", async () => {
// check namespaces work
    const clas = `
    CLASS /foo/cl_client DEFINITION PUBLIC.
      PUBLIC SECTION.
        CLASS-METHODS foo.
        CLASS-DATA bar TYPE i.
    ENDCLASS.
    CLASS /foo/cl_client IMPLEMENTATION.
      METHOD foo.
        CLEAR bar.
        bar = 2.
        WRITE 'expected'.
      ENDMETHOD.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        /foo/cl_client=>foo( ).
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "#foo#cl_client.clas.abap", contents: clas},
      {filename: "#foo#cl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-38", async () => {
// local class inheriting from global
    const sup = `
    CLASS cl_sup DEFINITION PUBLIC.
      PUBLIC SECTION.
        METHODS sup.
    ENDCLASS.
    CLASS cl_sup IMPLEMENTATION.
      METHOD sup.
        WRITE 'expected'.
      ENDMETHOD.
    ENDCLASS.`;

    const clas = `
    CLASS cl_client DEFINITION PUBLIC.
      PUBLIC SECTION.
    ENDCLASS.
    CLASS cl_client IMPLEMENTATION.
    ENDCLASS.`;
    const tests = `
    CLASS lcl DEFINITION INHERITING FROM cl_sup.
    ENDCLASS.
    CLASS lcl IMPLEMENTATION.
    ENDCLASS.

    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA ref TYPE REF TO lcl.
        CREATE OBJECT ref.
        ref->sup( ).
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "cl_client.clas.abap", contents: clas},
      {filename: "cl_sup.clas.abap", contents: sup},
      {filename: "cl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-39", async () => {
// should raise cx_sy_conversion_no_number

    const clas = `CLASS zcl_conv DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_conv IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test.
    DATA int TYPE i.
    TRY.
        int = '123-456'.
      CATCH cx_sy_conversion_no_number.
        WRITE 'expected'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.`;

    const cxconv = `
    CLASS cx_sy_conversion_no_number DEFINITION PUBLIC INHERITING FROM cx_root.
    ENDCLASS.
    CLASS cx_sy_conversion_no_number IMPLEMENTATION.
    ENDCLASS.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_conversion_no_number.clas.abap", contents: cxconv},
      {filename: "zcl_conv.clas.abap", contents: clas},
      {filename: "zcl_conv.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-40", async () => {
// assign constant from global interface

    const clas = `CLASS zcl_conv DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_conv IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test.
    DATA first TYPE string VALUE 'IF_INTF'.
    DATA second TYPE string VALUE 'foo'.
    FIELD-SYMBOLS <fs> TYPE any.
    ASSIGN (first)=>(second) TO <fs>.
    WRITE sy-subrc.
  ENDMETHOD.
ENDCLASS.`;

    const intf = `
INTERFACE if_intf PUBLIC.
  CONSTANTS foo TYPE i VALUE 2.
ENDINTERFACE.`;

    const files = [
      {filename: "if_intf.intf.abap", contents: intf},
      {filename: "zcl_assign.clas.abap", contents: clas},
      {filename: "zcl_assign.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("0");
  });

  it("test-41", async () => {
// class_constructor using constant value from interface

    const clas = `CLASS zcl_cc DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
ENDCLASS.

CLASS zcl_cc IMPLEMENTATION.
  METHOD class_constructor.
    WRITE zif_intf=>foo.
  ENDMETHOD.
ENDCLASS.`;

    const intf = `
INTERFACE zif_intf PUBLIC.
  CONSTANTS foo TYPE i VALUE 123.
ENDINTERFACE.`;

    const files = [
      {filename: "zcl_cc.clas.abap", contents: clas},
      {filename: "zif_intf.intf.abap", contents: intf},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[0]).to.equal("123");
  });

  it("test-42", async () => {
// escaping, namespaced class inheriting from namespaced class

    const clas1 = `CLASS /foo/cl_nn1 DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.
CLASS /foo/cl_nn1 IMPLEMENTATION.
ENDCLASS.`;

    const clas2 = `CLASS /foo/cl_nn2 DEFINITION PUBLIC INHERITING FROM /foo/cl_nn1.
  PUBLIC SECTION.
ENDCLASS.
CLASS /foo/cl_nn2 IMPLEMENTATION.
ENDCLASS.`;

    const files = [
      {filename: "#foo#cl_nn1.clas.abap", contents: clas1},
      {filename: "#foo#cl_nn2.clas.abap", contents: clas2},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[0]).to.equal("");
  });

  it("test-43", async () => {
// casting with interfaces, should throw cx_sy_move_cast_error
    const bar = `
    INTERFACE zif_bar PUBLIC.
    ENDINTERFACE.`;

    const clas = `
    CLASS zcl_foo DEFINITION PUBLIC.
    ENDCLASS.
    CLASS zcl_foo IMPLEMENTATION.
    ENDCLASS.`;

    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA foo TYPE REF TO object.
        DATA bar TYPE REF TO zif_bar.
        CREATE OBJECT foo TYPE zcl_foo.
        TRY.
            bar ?= foo.
          CATCH cx_sy_move_cast_error.
            WRITE 'expected'.
        ENDTRY.
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "cx_sy_move_cast_error.clas.abap", contents: cxmovecast},
      {filename: "zif_bar.intf.abap", contents: bar},
      {filename: "zcl_foo.clas.abap", contents: clas},
      {filename: "zcl_foo.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("expected");
  });

  it("test-44", async () => {
// casting with interfaces, ok
    const bar = `
    INTERFACE zif_bar PUBLIC.
    ENDINTERFACE.`;

    const clas = `
    CLASS zcl_foo DEFINITION PUBLIC.
      PUBLIC SECTION.
        INTERFACES zif_bar.
    ENDCLASS.
    CLASS zcl_foo IMPLEMENTATION.
    ENDCLASS.`;

    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA foo TYPE REF TO object.
        DATA bar TYPE REF TO zif_bar.
        CREATE OBJECT foo TYPE zcl_foo.
        bar ?= foo.
        WRITE 'ok'.
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "zif_bar.intf.abap", contents: bar},
      {filename: "zcl_foo.clas.abap", contents: clas},
      {filename: "zcl_foo.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("ok");
  });

  it("test-45", async () => {
// handling of JS keywords

    const clas = `
    CLASS zcl_keywords DEFINITION PUBLIC.
      PUBLIC SECTION.
        METHODS foo
          RETURNING VALUE(return) TYPE i.
        METHODS bar
          EXPORTING return TYPE i.
    ENDCLASS.
    CLASS zcl_keywords IMPLEMENTATION.
      METHOD foo.
        return = 2.
      ENDMETHOD.

      METHOD bar.
        return = 1.
      ENDMETHOD.
    ENDCLASS.`;

    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA obj TYPE REF TO zcl_keywords.
        DATA lv type i.
        CREATE OBJECT obj TYPE zcl_keywords.
        obj->foo( ).
        obj->bar( IMPORTING return = lv ).
        WRITE / 'ok'.
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "zcl_keywords.clas.abap", contents: clas},
      {filename: "zcl_keywords.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("ok");
  });

  it("test-46", async () => {
// CLAS locals includes and macros
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
        moo.
      ENDMETHOD.
    ENDCLASS.`;
    const macros = `
    DEFINE moo.
      WRITE 'moo'.
    END-OF-DEFINITION.`;

    const files = [
      {filename: "cx_root.clas.abap", contents: cxroot},
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "zcl_client.clas.macros.abap", contents: macros},
      {filename: "zcl_client.clas.locals_def.abap", contents: def},
      {filename: "zcl_client.clas.locals_imp.abap", contents: imp},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    const cons = await dumpNrun(files);
    expect(cons.split("\n")[1]).to.equal("from implmoo");
  });

  it("test-47", async () => {
// basic ENQU

    const files = [
      {filename: "ezlock.enqu.xml", contents: ezlock},
      {filename: "zlock.tabl.xml", contents: zlock},
    ];
    await dumpNrun(files);
  });

  it("test-48", async () => {
// namespaced function group with function module

    const fmname = `FUNCTION /foo/fmname.
  WRITE 'hello'.
ENDFUNCTION.`;
    const bartopabap = `FUNCTION-POOL /foo/bar.`;
    const bartopxml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>/FOO/LBARTOP</NAME>
    <DBAPL>S</DBAPL>
    <DBNA>D$</DBNA>
    <SUBC>I</SUBC>
    <APPL>S</APPL>
    <FIXPT>X</FIXPT>
    <LDBNAME>D$S</LDBNAME>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const saplbarabap = `INCLUDE /foo/lbartop.
INCLUDE /foo/lbaruxx.`;
    const saplbarxml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>/FOO/SAPLBAR</NAME>
    <DBAPL>S</DBAPL>
    <DBNA>D$</DBNA>
    <SUBC>F</SUBC>
    <APPL>S</APPL>
    <RLOAD>E</RLOAD>
    <FIXPT>X</FIXPT>
    <LDBNAME>D$S</LDBNAME>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const xml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_FUGR" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <AREAT>fugr</AREAT>
   <INCLUDES>
    <SOBJ_NAME>/FOO/LBARTOP</SOBJ_NAME>
    <SOBJ_NAME>/FOO/SAPLBAR</SOBJ_NAME>
   </INCLUDES>
   <FUNCTIONS>
    <item>
     <FUNCNAME>/FOO/FMNAME</FUNCNAME>
     <SHORT_TEXT>hello</SHORT_TEXT>
    </item>
   </FUNCTIONS>
  </asx:values>
 </asx:abap>
</abapGit>`;

    const files = [
      {filename: "#foo#bar.fugr.#foo#fmname.abap", contents: fmname},
      {filename: "#foo#bar.fugr.#foo#lbartop.abap", contents: bartopabap},
      {filename: "#foo#bar.fugr.#foo#lbartop.xml", contents: bartopxml},
      {filename: "#foo#bar.fugr.#foo#saplbar.abap", contents: saplbarabap},
      {filename: "#foo#bar.fugr.#foo#saplbar.xml", contents: saplbarxml},
      {filename: "#foo#bar.fugr.xml", contents: xml},
    ];
    await dumpNrun(files);
  });

  it("test-49", async () => {
// CREATE data, global class type

    const clas = `
    CLASS zcl_create DEFINITION PUBLIC.
      PUBLIC SECTION.
        TYPES ty_foo TYPE i.
    ENDCLASS.
    CLASS zcl_create IMPLEMENTATION.

    ENDCLASS.`;

    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA ref1 TYPE REF TO data.
        CREATE DATA ref1 TYPE zcl_create=>ty_foo.

        DATA ref2 TYPE REF TO data.
        CREATE DATA ref2 TYPE ('zcl_create=>ty_foo').
      ENDMETHOD.
    ENDCLASS.`;

    const files = [
      {filename: "zcl_create.clas.abap", contents: clas},
      {filename: "zcl_create.clas.testclasses.abap", contents: tests},
    ];
    await dumpNrun(files);
  });

  it("test-50", async () => {
// TYPE GROUP used in where
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
        TYPES: BEGIN OF ty,
                 cmptype TYPE c LENGTH 1,
               END OF ty.
        DATA table TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
        DATA row LIKE LINE OF table.
        LOOP AT table INTO row WHERE cmptype = seoo_cmptype_attribute.
        ENDLOOP.
      ENDMETHOD.
    ENDCLASS.`;
    const type = `TYPE-POOL seoo.
CONSTANTS seoo_cmptype_attribute TYPE n LENGTH 1 VALUE 'A'.`;
    const files = [
      {filename: "zcl_client.clas.abap", contents: clas},
      {filename: "seoo.type.abap", contents: type},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
    ];
    await dumpNrun(files);
  });

  it("test-51", async () => {
// INTF default parameter from constant
    const intf = `
INTERFACE zif_html PUBLIC.
  CONSTANTS:
    BEGIN OF c_action_type,
      sapevent TYPE c VALUE 'E',
    END OF c_action_type.

  METHODS a
    IMPORTING
      iv_typ TYPE c DEFAULT zif_html=>c_action_type-sapevent.
ENDINTERFACE.`;
    const clas = `
    CLASS zcl_html DEFINITION PUBLIC.
      PUBLIC SECTION.
        INTERFACES zif_html.
    ENDCLASS.
    CLASS zcl_html IMPLEMENTATION.
      METHOD zif_html~a.
        RETURN.
      ENDMETHOD.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
        METHODS foo
          IMPORTING
            iv_type TYPE c DEFAULT zif_html=>c_action_type-sapevent.
    ENDCLASS.
    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA ref TYPE REF TO zif_html.
        CREATE OBJECT ref TYPE zcl_html.
        ref->a( ).
        foo( ).
      ENDMETHOD.

      METHOD foo.
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "zcl_html.clas.abap", contents: clas},
      {filename: "zif_html.intf.abap", contents: intf},
      {filename: "zcl_html.clas.testclasses.abap", contents: tests},
    ];
    await dumpNrun(files);
  });

  it("test-52", async () => {
// constant from interface accessed in implementing class
    const intf = `
INTERFACE zif_html PUBLIC.
  CONSTANTS:
    BEGIN OF c_action_type,
      sapevent TYPE c VALUE 'E',
    END OF c_action_type.
ENDINTERFACE.`;
    const clas = `
    CLASS zcl_html DEFINITION PUBLIC.
      PUBLIC SECTION.
        INTERFACES zif_html.
        METHODS run.
    ENDCLASS.
    CLASS zcl_html IMPLEMENTATION.
      METHOD run.
        WRITE / zif_html~c_action_type-sapevent.
      ENDMETHOD.
    ENDCLASS.`;
    const tests = `
    CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
      PRIVATE SECTION.
        METHODS test01 FOR TESTING.
    ENDCLASS.

    CLASS ltcl_test IMPLEMENTATION.
      METHOD test01.
        DATA ref TYPE REF TO zcl_html.
        CREATE OBJECT ref TYPE zcl_html.
        ref->run( ).
      ENDMETHOD.
    ENDCLASS.`;
    const files = [
      {filename: "zcl_html.clas.abap", contents: clas},
      {filename: "zif_html.intf.abap", contents: intf},
      {filename: "zcl_html.clas.testclasses.abap", contents: tests},
    ];
    await dumpNrun(files);
  });

  it("test-53", async () => {
// class constructor in locals using global

    const clas = `
CLASS zcl_html DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.
CLASS zcl_html IMPLEMENTATION.
ENDCLASS.`;

    const locals = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD class_constructor.
    DATA ref TYPE REF TO zcl_html.
    CREATE OBJECT ref.
    WRITE 'hello hello'.
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "zcl_html.clas.locals.abap", contents: locals},
      {filename: "zcl_html.clas.abap", contents: clas},
    ];
    await dumpNrun(files);
  });

  it("test-54", async () => {
// MESSAGE without database connection

    const msag_zag_unit_test = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_MSAG" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <T100A>
    <ARBGB>ZAG_UNIT_TEST</ARBGB>
    <MASTERLANG>E</MASTERLANG>
    <STEXT>test</STEXT>
   </T100A>
   <T100>
    <T100>
     <SPRSL>E</SPRSL>
     <ARBGB>ZAG_UNIT_TEST</ARBGB>
     <MSGNR>000</MSGNR>
     <TEXT>hello world</TEXT>
    </T100>
   </T100>
  </asx:values>
 </asx:abap>
</abapGit>`;

    const clas = `
CLASS zcl_html DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.
CLASS zcl_html IMPLEMENTATION.
ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.
    METHODS test01 FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test01.
    DATA lv_str TYPE string.
    MESSAGE e000(zag_unit_test) INTO lv_str.
    ASSERT lv_str = 'hello world'.
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test},
      {filename: "zcl_html.clas.testclasses.abap", contents: tests},
      {filename: "zcl_html.clas.abap", contents: clas},
    ];
    await dumpNrun(files, false);
  });

  it("test-55", async () => {
    // CREATE DATA dynamic global interface

    const intf = `
    INTERFACE if_bar PUBLIC.
      CONSTANTS value TYPE i VALUE 2.
    ENDINTERFACE.`;

    const clas = `
    CLASS zcl_html DEFINITION PUBLIC.
      PUBLIC SECTION.
    ENDCLASS.
    CLASS zcl_html IMPLEMENTATION.
    ENDCLASS.`;

    const tests = `
CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.
    METHODS test01 FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test01.
    DATA lr_data TYPE REF TO data.
    CREATE DATA lr_data TYPE REF TO ('IF_BAR').
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "zcl_html.clas.testclasses.abap", contents: tests},
      {filename: "if_bar.intf.abap", contents: intf},
      {filename: "zcl_html.clas.abap", contents: clas},
    ];
    await dumpNrun(files, false);
  });

  it.only("test-56", async () => {
    // State via top variable in function group

    const file1 = `class ZCL_FUGR_TEST definition public final create public .
  public section.
  protected section.
  private section.
ENDCLASS.

CLASS ZCL_FUGR_TEST IMPLEMENTATION.
ENDCLASS.`;

    const file2 = `CLASS ltcl_Test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.
  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.


CLASS ltcl_Test IMPLEMENTATION.

  METHOD test.

    DATA lc_value TYPE i VALUE 34.
    DATA lv_value TYPE i.

    CALL FUNCTION 'ZFUGR_SET'
      EXPORTING
        value = lc_value.

    CALL FUNCTION 'ZFUGR_GET'
      IMPORTING
        value = lv_value.

    ASSERT lv_value = lc_value.

  ENDMETHOD.

ENDCLASS.`;

    const file3 = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_FUGR_TEST</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>test</DESCRIPT>
    <STATE>1</STATE>
    <CLSCCINCL>X</CLSCCINCL>
    <FIXPT>X</FIXPT>
    <UNICODE>X</UNICODE>
    <WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>`;

    const file4 = `FUNCTION-POOL zfugr.                        "MESSAGE-ID ..

* INCLUDE LZFUGRD...                         " Local class definition

DATA int TYPE i.`;

    const file5 = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>LZFUGRTOP</NAME>
    <DBAPL>S</DBAPL>
    <DBNA>D$</DBNA>
    <SUBC>I</SUBC>
    <APPL>S</APPL>
    <FIXPT>X</FIXPT>
    <LDBNAME>D$S</LDBNAME>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>`;

    const file6 = `
  INCLUDE LZFUGRTOP.                         " Global Declarations
  INCLUDE LZFUGRUXX.                         " Function Modules`;

    const file7 = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>SAPLZFUGR</NAME>
    <DBAPL>S</DBAPL>
    <DBNA>D$</DBNA>
    <SUBC>F</SUBC>
    <APPL>S</APPL>
    <RLOAD>E</RLOAD>
    <FIXPT>X</FIXPT>
    <LDBNAME>D$S</LDBNAME>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>`;

    const file8 = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_FUGR" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <AREAT>test</AREAT>
   <INCLUDES>
    <SOBJ_NAME>LZFUGRTOP</SOBJ_NAME>
    <SOBJ_NAME>SAPLZFUGR</SOBJ_NAME>
   </INCLUDES>
   <FUNCTIONS>
    <item>
     <FUNCNAME>ZFUGR_GET</FUNCNAME>
     <SHORT_TEXT>get</SHORT_TEXT>
     <EXPORT>
      <RSEXP>
       <PARAMETER>VALUE</PARAMETER>
       <TYP>I</TYP>
      </RSEXP>
     </EXPORT>
     <DOCUMENTATION>
      <RSFDO>
       <PARAMETER>VALUE</PARAMETER>
       <KIND>P</KIND>
      </RSFDO>
     </DOCUMENTATION>
    </item>
    <item>
     <FUNCNAME>ZFUGR_SET</FUNCNAME>
     <SHORT_TEXT>set</SHORT_TEXT>
     <IMPORT>
      <RSIMP>
       <PARAMETER>VALUE</PARAMETER>
       <REFERENCE>X</REFERENCE>
       <TYP>I</TYP>
      </RSIMP>
     </IMPORT>
     <DOCUMENTATION>
      <RSFDO>
       <PARAMETER>VALUE</PARAMETER>
       <KIND>P</KIND>
      </RSFDO>
     </DOCUMENTATION>
    </item>
   </FUNCTIONS>
  </asx:values>
 </asx:abap>
</abapGit>`;

    const file9 = `FUNCTION zfugr_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(VALUE) TYPE  I
*"----------------------------------------------------------------------

  value = int.

ENDFUNCTION.`;

    const file10 = `FUNCTION zfugr_set.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(VALUE) TYPE  I
*"----------------------------------------------------------------------

  int = value.

ENDFUNCTION.`;

    const files = [
      {filename: "zcl_fugr_test.clas.abap", contents: file1},
      {filename: "zcl_fugr_test.clas.testclasses.abap", contents: file2},
      {filename: "zcl_fugr_test.clas.xml", contents: file3},
      {filename: "zfugr.fugr.lzfugrtop.abap", contents: file4},
      {filename: "zfugr.fugr.lzfugrtop.xml", contents: file5},
      {filename: "zfugr.fugr.saplzfugr.abap", contents: file6},
      {filename: "zfugr.fugr.saplzfugr.xml", contents: file7},
      {filename: "zfugr.fugr.xml", contents: file8},
      {filename: "zfugr.fugr.zfugr_get.abap", contents: file9},
      {filename: "zfugr.fugr.zfugr_set.abap", contents: file10},
    ];
    await dumpNrun(files, false);
  });

});
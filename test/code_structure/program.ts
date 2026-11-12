import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, compileFiles, runFiles} from "../_utils";

function progXml(name: string, subc: "1" | "I") {
  return `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_PROG" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>${name.toUpperCase()}</NAME>
    <SUBC>${subc}</SUBC>
    <RSTAT>K</RSTAT>
    <RLOAD>E</RLOAD>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>`;
}

describe("Running code structure - Program", () => {

  it("a include in itself should be skipped", async () => {
    await compileFiles([
      {filename: "zinclude.prog.abap", contents: `DATA foo TYPE i.`},
      {filename: "zinclude.prog.xml", contents: progXml("zinclude", "I")},
    ]);
  });

  it("INCLUDE in program is expanded, top + forms + nested", async () => {
    const main = `REPORT zrepro.

INCLUDE zrepro_top.

START-OF-SELECTION.
  PERFORM run.
  WRITE / gv_value.
  WRITE / gv_nested.

INCLUDE zrepro_f01.`;
    const top = `TYPES ty_int TYPE i.
DATA gv_value TYPE ty_int.
INCLUDE zrepro_nested.`;
    const nested = `DATA gv_nested TYPE string VALUE 'nested'.`;
    const f01 = `FORM run.
  gv_value = gv_value + 1.
ENDFORM.`;

    const abap = new ABAP({console: new MemoryConsole()});
    const js = await runFiles(abap, [
      {filename: "zrepro.prog.abap", contents: main},
      {filename: "zrepro.prog.xml", contents: progXml("zrepro", "1")},
      {filename: "zrepro_top.prog.abap", contents: top},
      {filename: "zrepro_top.prog.xml", contents: progXml("zrepro_top", "I")},
      {filename: "zrepro_nested.prog.abap", contents: nested},
      {filename: "zrepro_nested.prog.xml", contents: progXml("zrepro_nested", "I")},
      {filename: "zrepro_f01.prog.abap", contents: f01},
      {filename: "zrepro_f01.prog.xml", contents: progXml("zrepro_f01", "I")},
    ]);
    expect(js).to.not.include("transpiler TODO: INCLUDE");
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\nnested");
  });

  it("local classes and forms from INCLUDEs are exported from the program", async () => {
    const main = `REPORT zrepro.
INCLUDE zrepro_top.
START-OF-SELECTION.
  PERFORM run.
INCLUDE zrepro_f01.`;
    const top = `DATA gv_value TYPE i.
INCLUDE zrepro_t01.`;
    const f01 = `FORM run.
  gv_value = gv_value + 1.
ENDFORM.`;
    const t01 = `CLASS ltcl_test DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test_run FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_run.
    CLEAR gv_value.
    PERFORM run.
  ENDMETHOD.
ENDCLASS.`;

    const res = await compileFiles([
      {filename: "zrepro.prog.abap", contents: main},
      {filename: "zrepro.prog.xml", contents: progXml("zrepro", "1")},
      {filename: "zrepro_top.prog.abap", contents: top},
      {filename: "zrepro_top.prog.xml", contents: progXml("zrepro_top", "I")},
      {filename: "zrepro_f01.prog.abap", contents: f01},
      {filename: "zrepro_f01.prog.xml", contents: progXml("zrepro_f01", "I")},
      {filename: "zrepro_t01.prog.abap", contents: t01},
      {filename: "zrepro_t01.prog.xml", contents: progXml("zrepro_t01", "I")},
    ], {addCommonJS: true});
    const prog = res.objects.find(o => o.object.name === "ZREPRO");
    expect(prog).to.not.equal(undefined);
    expect(prog!.exports).to.have.members(["ltcl_test", "form$run", "$main"]);
    expect(prog!.chunk.getCode()).to.include("export {ltcl_test, form$run, $main};");
  });

});

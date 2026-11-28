import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

// the data elements INT1 and INT2 as on a system, the built-in types b and s
function dataElement(name: string, length: string, outputLength: string) {
  return `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD04V>
    <ROLLNAME>${name}</ROLLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>${name}</DDTEXT>
    <DTELMASTER>E</DTELMASTER>
    <DATATYPE>${name}</DATATYPE>
    <LENG>${length}</LENG>
    <OUTPUTLEN>${outputLength}</OUTPUTLEN>
   </DD04V>
  </asx:values>
 </asx:abap>
</abapGit>`;
}

async function run(contents: string) {
  const js = await runFiles(abap, [
    {filename: "zfoobar.prog.abap", contents},
    {filename: "int1.dtel.xml", contents: dataElement("INT1", "000003", "000003")},
    {filename: "int2.dtel.xml", contents: dataElement("INT2", "000005", "000006")}]);
  const f = new AsyncFunction("abap", js);
  await f(abap);
}

// Operations on INT1 and INT2 inside their ranges, each snippet run on a system (ABAP 816)
// with the same ASSERTs; the ranges themselves are #2199
describe("Running Examples - INT1 and INT2 operations", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("the limits of the ranges, 0 and 255 for INT1, -32768 and 32767 for INT2", async () => {
    await run(`
DATA lv_b TYPE int1.
DATA lv_s TYPE int2.
lv_b = 255.
ASSERT lv_b = 255.
lv_b = 0.
ASSERT lv_b = 0.
lv_s = 32767.
ASSERT lv_s = 32767.
lv_s = -32768.
ASSERT lv_s = -32768.`);
  });

  it("calculation type i: 200 + 100 into i, / rounds half away from zero, from f rounded", async () => {
    await run(`
DATA lv_b TYPE int1 VALUE 200.
DATA lv_b2 TYPE int1 VALUE 100.
DATA lv_s TYPE int2.
DATA lv_s2 TYPE int2 VALUE 2.
DATA lv_i TYPE i.
DATA lv_f TYPE f.
lv_i = lv_b + lv_b2.
ASSERT lv_i = 300.
lv_b = 7.
lv_b2 = 2.
lv_b = lv_b / lv_b2.
ASSERT lv_b = 4.
lv_s = -7.
lv_s = lv_s / lv_s2.
ASSERT lv_s = -4.
lv_s = 1000.
lv_i = lv_s * lv_s.
ASSERT lv_i = 1000000.
lv_f = '2.5'.
lv_b = lv_f.
ASSERT lv_b = 3.
lv_f = '-0.4'.
lv_b = lv_f.
ASSERT lv_b = 0.`);
  });

  it("to c with the sign last, to x and back, string template", async () => {
    await run(`
DATA lv_b TYPE int1 VALUE 200.
DATA lv_s TYPE int2 VALUE -300.
DATA lv_c TYPE c LENGTH 8.
DATA lv_x1 TYPE x LENGTH 1.
DATA lv_x2 TYPE x LENGTH 2.
lv_c = lv_s.
ASSERT lv_c = '    300-'.
lv_c = lv_b.
ASSERT lv_c = '    200 '.
lv_x1 = lv_b.
ASSERT lv_x1 = 'C8'.
lv_x2 = lv_s.
ASSERT lv_x2 = 'FED4'.
lv_x1 = 'FF'.
lv_b = lv_x1.
ASSERT lv_b = 255.
lv_x2 = '7FFF'.
lv_s = lv_x2.
ASSERT lv_s = 32767.
lv_s = -300.
ASSERT |{ lv_s }| = \`-300\`.`);
  });

  it("comparisons with i, INT1 and text", async () => {
    await run(`
DATA lv_b TYPE int1 VALUE 200.
DATA lv_s TYPE int2 VALUE -300.
DATA lv_i TYPE i VALUE 199.
ASSERT lv_b > lv_i.
ASSERT lv_s < lv_b.
ASSERT lv_s = '-300'.`);
  });

  it("COLLECT sums, READ TABLE WITH KEY and SORT on an INT2 component", async () => {
    await run(`
TYPES: BEGIN OF ty_row,
         key    TYPE c LENGTH 1,
         amount TYPE int2,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_row TYPE ty_row.
DATA lv_keys TYPE string.
ls_row-key = 'B'.
ls_row-amount = -11.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'A'.
ls_row-amount = 22.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'B'.
ls_row-amount = 4.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'C'.
ls_row-amount = -30.
COLLECT ls_row INTO lt_rows.
ASSERT lines( lt_rows ) = 3.
READ TABLE lt_rows INTO ls_row WITH KEY key = 'B'.
ASSERT ls_row-amount = -7.
SORT lt_rows BY amount.
LOOP AT lt_rows INTO ls_row.
  lv_keys = lv_keys && ls_row-key.
ENDLOOP.
ASSERT lv_keys = \`CBA\`.`);
  });

});

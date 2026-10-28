import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - GT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("empty xstr gt x", async () => {
    const code = `
data foo type xstring.
data bar type x length 1.
if foo > bar.
  write 'yes'.
ELSE.
  WRITE 'no'.
endif.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`no`);
  });

  it("GT char structured", async () => {
    const code = `
CONSTANTS:
  BEGIN OF c_package_file,
    obj_name  TYPE c LENGTH 7 VALUE 'package',
    sep1      TYPE c LENGTH 1 VALUE '.',
    obj_type  TYPE c LENGTH 4 VALUE 'devc',
    sep2      TYPE c LENGTH 1 VALUE '.',
    extension TYPE c LENGTH 3 VALUE 'xml',
  END OF c_package_file.

ASSERT c_package_file GT 'foo'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("GT and LT, structure vs structure", async () => {
    const code = `
TYPES: BEGIN OF ty,
         a TYPE c LENGTH 3,
         b TYPE i,
       END OF ty.
DATA small TYPE ty.
DATA big TYPE ty.
small-a = 'AAA'.
small-b = 5.
big-a = 'AAA'.
big-b = 7.
ASSERT big > small.
ASSERT small < big.
ASSERT NOT small > big.
ASSERT NOT big < small.
ASSERT big >= small.
ASSERT small <= big.
ASSERT NOT small > small.
ASSERT small >= small.
big-a = 'AAB'.
big-b = 1.
ASSERT big > small.
ASSERT small < big.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("GT and LT, nested structure", async () => {
    const code = `
TYPES: BEGIN OF ty_inner,
         type TYPE c LENGTH 4,
         name TYPE c LENGTH 10,
       END OF ty_inner.
TYPES: BEGIN OF ty,
         inner TYPE ty_inner,
         file  TYPE string,
       END OF ty.
DATA small TYPE ty.
DATA big TYPE ty.
small-inner-type = 'CLAS'.
small-inner-name = 'ZCL_A'.
small-file = 'z'.
big-inner-type = 'CLAS'.
big-inner-name = 'ZCL_B'.
big-file = 'a'.
ASSERT big > small.
ASSERT small < big.
ASSERT NOT small > big.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
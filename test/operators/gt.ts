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

});
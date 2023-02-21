// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - EQ", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("EQ char vs structure", async () => {
    const code = `
CONSTANTS:
  BEGIN OF c_package_file,
    obj_name  TYPE c LENGTH 7 VALUE 'package',
    sep1      TYPE c LENGTH 1 VALUE '.',
    obj_type  TYPE c LENGTH 4 VALUE 'devc',
    sep2      TYPE c LENGTH 1 VALUE '.',
    extension TYPE c LENGTH 3 VALUE 'xml',
  END OF c_package_file.

DATA str TYPE string.
str = |package.devc.xml|.

ASSERT 'package.devc.xml' = c_package_file.
ASSERT c_package_file = 'package.devc.xml'.
ASSERT str = c_package_file.
ASSERT c_package_file = str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
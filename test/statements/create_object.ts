import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CREATE OBJECT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("CREATE OBJECT, should create new references", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    DATA value TYPE i.
    METHODS constructor IMPORTING val TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD constructor.
    value = val.
  ENDMETHOD.
ENDCLASS.

FORM foo.
  DATA ref TYPE REF TO lcl_bar.
  DATA tab TYPE STANDARD TABLE OF REF TO lcl_bar.
  CREATE OBJECT ref EXPORTING val = 1.
  APPEND ref TO tab.
  CREATE OBJECT ref EXPORTING val = 2.
  APPEND ref TO tab.
  LOOP AT tab INTO ref.
    WRITE / ref->value.
  ENDLOOP.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

});
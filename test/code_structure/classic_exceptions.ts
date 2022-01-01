// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running code structure - Classic Exceptions", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Classic exceptions, class", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS send
      EXCEPTIONS
        http_communication_failure.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD send.
    FIND 'foo' IN 'bar'.
    ASSERT sy-subrc = 4.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>send(
    EXCEPTIONS
      http_communication_failure = 1
      OTHERS                     = 5 ).
  ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Classic exceptions, interface", async () => {
    const code = `
INTERFACE lif.
CLASS-METHODS send
  EXCEPTIONS
    http_communication_failure.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD lif~send.
    FIND 'foo' IN 'bar'.
    ASSERT sy-subrc = 4.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>lif~send(
    EXCEPTIONS
      http_communication_failure = 1
      OTHERS                     = 5 ).
  ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
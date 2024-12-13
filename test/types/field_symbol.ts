import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Field Symbol type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("test", async () => {
    const code = `
DATA: BEGIN OF stru1,
        bar TYPE i,
      END OF stru1.
DATA: BEGIN OF stru2,
        bar TYPE i,
      END OF stru2.
FIELD-SYMBOLS <stru1> TYPE any.
FIELD-SYMBOLS <stru2> TYPE any.
stru1-bar = 2.
ASSIGN stru1 TO <stru1>.
ASSIGN stru2 TO <stru2>.
<stru2> = <stru1>.
WRITE stru2-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("assigning via object", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref1 TYPE REF TO lcl.
  DATA ref2 TYPE REF TO lcl.
  FIELD-SYMBOLS <fs> TYPE any.
  CREATE OBJECT ref1.
  ASSIGN ref1 TO <fs>.
  ref2 = <fs>.
  ref2->foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("fs and references and string and integer", async () => {
    const code = `
DATA int TYPE i.
DATA min_val TYPE REF TO i.
FIELD-SYMBOLS <min> TYPE any.
int = -10.
GET REFERENCE OF int INTO min_val.
ASSIGN min_val->* TO <min>.
DATA min_str TYPE string.
min_str = <min>.
WRITE min_str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10-");
  });

  it.only("fs body tests", async () => {
    const code = `
DATA foo TYPE STANDARD TABLE OF string.
DATA bar LIKE foo.
DATA baz LIKE foo.
FIELD-SYMBOLS <baz> LIKE baz.

ASSIGN baz TO <baz>.

APPEND 'arf' TO foo.
APPEND 'meh' TO foo.
APPEND 'woo' TO foo.

bar = foo.
bar[] = foo[].
<baz> = foo.
<baz>[] = foo[].`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    // just test it runs
  });

});
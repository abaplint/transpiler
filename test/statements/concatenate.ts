import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CONCATENATE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Basic CONCATENATE", async () => {
    const code = `
      DATA target TYPE string.
      CONCATENATE 'foo' 'bar' INTO target.
      ASSERT target = 'foobar'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CONCATENATE SEPARATED BY space", async () => {
    const code = `
      DATA lv_string TYPE string.
      DATA lv_char10 TYPE c LENGTH 10.
      DATA iv_type TYPE c LENGTH 6 VALUE 'commit'.
      lv_char10 = 6.
      CONDENSE lv_char10.
      CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
      ASSERT lv_string = 'commit 6'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CONCATENATE LINES OF", async () => {
    const code = `
      DATA rv_html TYPE string.
      DATA lt_temp TYPE STANDARD TABLE OF string.
      APPEND 'fo' TO lt_temp.
      APPEND 'bar' TO lt_temp.
      CONCATENATE LINES OF lt_temp INTO rv_html SEPARATED BY 'o'.
      ASSERT rv_html = 'foobar'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CONCATENATE, respecting blanks", async () => {
    const code = `
    DATA character TYPE c LENGTH 1.
    DATA lv_str TYPE string.
    character = ' '.
    CONCATENATE lv_str character INTO lv_str RESPECTING BLANKS.
    WRITE strlen( lv_str ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("more CONCATENATE", async () => {
    const code = `
    DATA foo TYPE c LENGTH 2.
    DATA bar TYPE c LENGTH 2.
    DATA res TYPE c LENGTH 4.
    foo = '1'.
    bar = '2'.
    CONCATENATE foo bar INTO res.
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12  ");
  });

  it("spaces, spaces", async () => {
    const code = `
    DATA foo TYPE c LENGTH 2.
    CONCATENATE foo '1' INTO foo.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1 ");
  });

  it("spaces, spaces and strings", async () => {
    const code = `
    DATA foo TYPE string.
    DATA bar TYPE string.
    DATA res TYPE string.
    foo = |hello |.
    bar = |world|.
    CONCATENATE foo bar INTO res.
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello world");
  });

  it("new line", async () => {
    const code = `
    DATA foo TYPE c LENGTH 1.
    DATA bar TYPE string.
    DATA res TYPE string.
    foo = |\\n|.
    bar = |1|.
    CONCATENATE foo bar INTO res.
    ASSERT strlen( res ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("concatenate, blanks", async () => {
    const code = `
DATA input TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA count TYPE i.
DATA field TYPE string.
APPEND \`  \` TO input.
APPEND \`   \` TO input.
CONCATENATE LINES OF input INTO field RESPECTING BLANKS.
count = strlen( field ).
WRITE count.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("Separated by INTF constant", async () => {
    const code = `
INTERFACE lif.
  CONSTANTS sep TYPE string VALUE '-'.
ENDINTERFACE.

DATA lt_version_parts TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA res TYPE string.
FIELD-SYMBOLS <l_version_part> LIKE LINE OF lt_version_parts.

INSERT \`foo\` INTO TABLE lt_version_parts.
INSERT \`bar\` INTO TABLE lt_version_parts.
CONCATENATE LINES OF lt_version_parts INTO res SEPARATED BY lif=>sep.
WRITE / res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo-bar");
  });

});
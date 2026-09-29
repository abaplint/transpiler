import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - IN", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("IN empty", async () => {
    const code = `
  DATA bar TYPE RANGE OF i.
  ASSERT 5 IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NOT IN", async () => {
    const code = `
  DATA bar TYPE RANGE OF i.
  FIELD-SYMBOLS <moo> LIKE LINE OF bar.
  APPEND INITIAL LINE TO bar ASSIGNING <moo>.
  <moo>-sign = 'I'.
  <moo>-option = 'EQ'.
  <moo>-low = 2.
  ASSERT 5 NOT IN bar.
  ASSERT NOT 5 IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("2 IN 2", async () => {
    const code = `
  DATA bar TYPE RANGE OF i.
  FIELD-SYMBOLS <moo> LIKE LINE OF bar.
  APPEND INITIAL LINE TO bar ASSIGNING <moo>.
  <moo>-sign = 'I'.
  <moo>-option = 'EQ'.
  <moo>-low = 2.
  ASSERT 2 IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("IN with I CP", async () => {
    const code = `
DATA bar TYPE RANGE OF string.
FIELD-SYMBOLS <moo> LIKE LINE OF bar.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'CP'.
<moo>-low = '*hello*'.
ASSERT 'hello world' IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("E EQ", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
DATA range TYPE RANGE OF i.
DATA rr LIKE LINE OF range.

DO 3 TIMES.
  APPEND sy-index TO tab.
ENDDO.

rr-sign = 'E'.
rr-option = 'EQ'.
rr-low = 2.
APPEND rr TO range.

LOOP AT tab INTO row WHERE table_line IN range.
  WRITE / row.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n3");
  });


  it("every option, I sign", async () => {
    const code = `
DATA range TYPE RANGE OF i.
DATA rr LIKE LINE OF range.
DEFINE _check.
  CLEAR range.
  rr-sign = 'I'. rr-option = &1. rr-low = &2. rr-high = &3.
  APPEND rr TO range.
  IF &4 IN range. WRITE / 'Y'. ELSE. WRITE / 'N'. ENDIF.
END-OF-DEFINITION.
_check 'EQ' 5 0 5.
_check 'NE' 5 0 5.
_check 'GT' 5 0 6.
_check 'GE' 5 0 5.
_check 'LT' 5 0 4.
_check 'LE' 5 0 6.
_check 'BT' 3 5 4.
_check 'BT' 3 5 6.
_check 'NB' 3 5 6.
_check 'NB' 3 5 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("Y\nN\nY\nY\nY\nN\nY\nN\nY\nN");
  });

  it("NP", async () => {
    const code = `
DATA range TYPE RANGE OF string.
DATA rr LIKE LINE OF range.
rr-sign = 'I'. rr-option = 'NP'. rr-low = 'a*'.
APPEND rr TO range.
ASSERT 'banana' IN range.
ASSERT NOT 'apple' IN range.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("I BT with an E EQ hole", async () => {
    const code = `
DATA range TYPE RANGE OF i.
DATA rr LIKE LINE OF range.
rr-sign = 'I'. rr-option = 'BT'. rr-low = 3. rr-high = 5.
APPEND rr TO range.
CLEAR rr.
rr-sign = 'E'. rr-option = 'EQ'. rr-low = 4.
APPEND rr TO range.
DO 7 TIMES.
  IF sy-index IN range.
    WRITE / sy-index.
  ENDIF.
ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n5");
  });

  it("an E row does not admit a value no I row admits", async () => {
    const code = `
DATA range TYPE RANGE OF i.
DATA rr LIKE LINE OF range.
rr-sign = 'I'. rr-option = 'EQ'. rr-low = 7.
APPEND rr TO range.
rr-sign = 'E'. rr-option = 'EQ'. rr-low = 4.
APPEND rr TO range.
ASSERT 7 IN range.
ASSERT NOT 1 IN range.
ASSERT NOT 4 IN range.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("E rows only admit what they do not exclude", async () => {
    const code = `
DATA range TYPE RANGE OF i.
DATA rr LIKE LINE OF range.
rr-sign = 'E'. rr-option = 'BT'. rr-low = 2. rr-high = 3.
APPEND rr TO range.
ASSERT 1 IN range.
ASSERT NOT 2 IN range.
ASSERT NOT 3 IN range.
ASSERT 4 IN range.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
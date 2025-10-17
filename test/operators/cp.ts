import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - CP", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("structured CP compare", async () => {
    const code = `
TYPES: BEGIN OF ty,
         line TYPE c LENGTH 200,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lv_line LIKE LINE OF tab.

APPEND '* regenerated at 06.06.2022 10:47:40' TO tab.

LOOP AT tab INTO lv_line.
  IF lv_line CP '#**regenerated at *'.
    WRITE 'yes'.
  ELSE.
    WRITE 'no'.
  ENDIF.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("structured CP compare, via field symbol", async () => {
    const code = `
TYPES: BEGIN OF ty,
         line TYPE c LENGTH 200,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
FIELD-SYMBOLS <line> LIKE LINE OF tab.

APPEND '* regenerated at 06.06.2022 10:47:40' TO tab.

LOOP AT tab ASSIGNING <line>.
  IF <line> CP '#**regenerated at *'.
    WRITE 'yes'.
  ELSE.
    WRITE 'no'.
  ENDIF.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("CP", async () => {
    const code = `
      DATA bar TYPE string.
      bar = 'abc'.
      ASSERT bar CP 'a*'.
      ASSERT bar CP 'A*'.
      ASSERT bar CP '*b*'.
      ASSERT bar CP '*c'.
      ASSERT bar CP 'abc'.
      ASSERT bar CP '*abc*'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CP 2", async () => {
    const code = `ASSERT |comment\\n| CP 'comment+'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CP, whitespace", async () => {
    const code = `ASSERT |hello\\nfoobar\\nmoo| CP '*oo*'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("more CP", async () => {
    const code = `
    ASSERT '<?xml' CP '<?xml'.
    ASSERT '<?xml sdf' CP '<?xml *'.
    ASSERT '()' CP '()'.
    ASSERT '[]' CP '[]'.
    ASSERT '.' CP '.'.
    ASSERT '|' CP '|'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CP with slash", async () => {
    const code = `
    DATA absolute_name TYPE string.
    absolute_name = '\\TYPE=FOO'.
    ASSERT absolute_name CP '\\TYPE=*'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CP stuff", async () => {
    const code = `
  DATA lt_text TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  APPEND '01.01.0001 foo' TO lt_text.
  APPEND '213  123 456456' TO lt_text.
  APPEND 'aa 01.01.0001 foo' TO lt_text.
  DELETE lt_text WHERE table_line CP |++.++.++++ *|.
  WRITE lines( lt_text ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`2`);
  });

  it("CP, escaping", async () => {
    const code = `
    DATA lv_text TYPE string.
    lv_text = '* regen'.
    IF lv_text CP '#**regen'.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it("CP, bracket", async () => {
    const code = `
    DATA lv_text TYPE string.
    lv_text = 'foo{'.
    IF lv_text CP '*{'.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it("CP, dollar", async () => {
    const code = `
    DATA lv_val1 TYPE string.
    DATA lv_val2 TYPE string.
    lv_val1 = |Package $MAIN_SUB already exists but is not a sub-package of $MAIN. Check|.
    lv_val2 = |Package $MAIN_SUB already exists but is not a sub-package of $MAIN*|.
    IF lv_val1 CP lv_val2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it("CP, spaces", async () => {
    const code = `
    DATA foo1 TYPE c LENGTH 5.
    DATA foo2 TYPE c LENGTH 5.
    foo1 = 'ABC'.
    foo2 = '*'.
    ASSERT foo1 CP foo2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CP, more spaces", async () => {
    const code = `
    DATA foo1 TYPE c LENGTH 5.
    DATA foo2 TYPE c LENGTH 5.
    foo1 = 'ABC'.
    foo2 = 'AB*'.
    ASSERT foo1 CP foo2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CP, more spaces 2", async () => {
    const code = `
    DATA foo TYPE c LENGTH 200.
    foo = 'CLASS=LIF_TEST_TYPESTYPE=FOO'.
    ASSERT foo CP '*TYPE=FOO'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CP, octothorpe logic", async () => {
    const code = `ASSERT '#name#' CP '##name##'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
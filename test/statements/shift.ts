import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - SHIFT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("shift 1 places left", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abc'.
  SHIFT lv_temp BY 1 PLACES LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bc");
  });

  it("shift in byte mode", async () => {
    const code = `
    DATA lv_xstring TYPE xstring.
    lv_xstring = '0061736D' .
    SHIFT lv_xstring IN BYTE MODE BY 1 PLACES CIRCULAR.
    write lv_xstring+0(1).
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("61");
  });

  it("shift up to left, found", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abc/bar'.
  SHIFT lv_temp UP TO '/' LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("/bar");
  });

  it("shift up to left, not found", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abcbar'.
  SHIFT lv_temp UP TO '/' LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abcbar");
  });

  it("SHIFT LEFT", async () => {
    const code = `
  DATA foo TYPE c LENGTH 10.
  foo = '11223355'.
  SHIFT foo LEFT DELETING LEADING '12'.
  WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3355      ");
  });

  it("SHIFT LEFT CIRCULAR", async () => {
    const code = `
  DATA lv_c TYPE c LENGTH 4.
  lv_c = 'ABCD'.
  SHIFT lv_c LEFT BY 2 PLACES CIRCULAR.
  WRITE / lv_c.
  SHIFT lv_c LEFT BY 1 PLACES CIRCULAR.
  WRITE / lv_c.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("CDAB\nDABC");
  });

  it("shift left", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abc'.
  SHIFT lv_temp LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bc");
  });

  it("shift left, empty str", async () => {
    const code = `
  DATA lv_temp TYPE string.
  SHIFT lv_temp LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SHIFT LEFT IN BYTE MODE", async () => {
    const code = `
    DATA foo TYPE x LENGTH 10.
    foo = '1122'.
    SHIFT foo LEFT IN BYTE MODE.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("22000000000000000000");
  });

  it("SHIFT LEFT CIRCULAR IN BYTE MODE", async () => {
    const code = `
    DATA foo TYPE x LENGTH 10.
    foo = '1122'.
    SHIFT foo LEFT CIRCULAR IN BYTE MODE.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("22000000000000000011");
  });

  it("SHIFT RIGHT DELETING TRAILING", async () => {
    const code = `
    DATA text TYPE string.
    text = |foo |.
    SHIFT text RIGHT DELETING TRAILING | |.
    ASSERT text = | foo|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SHIFT RIGHT/LEFT DELETING TRAILING", async () => {
    const code = `
  DATA string_to_work_on TYPE string.
  string_to_work_on = \`Title  \`.
  SHIFT string_to_work_on RIGHT DELETING TRAILING space.
  ASSERT string_to_work_on = \`  Title\`.
  SHIFT string_to_work_on LEFT DELETING LEADING space.
  ASSERT string_to_work_on = \`Title\`.
  ASSERT strlen( string_to_work_on ) = 5.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SHIFT DELETING TRAILING space space", async () => {
    const code = `
    DATA string_to_work_on TYPE string.
    string_to_work_on = \` \`.
    SHIFT string_to_work_on RIGHT DELETING TRAILING space.
    ASSERT string_to_work_on = \` \`.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SHIFT DELETING TRAILING one one", async () => {
    const code = `
    DATA string_to_work_on TYPE string.
    string_to_work_on = \`1\`.
    SHIFT string_to_work_on RIGHT DELETING TRAILING '1'.
    ASSERT string_to_work_on = \` \`.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SHIFT DELETING TRAILING spaced up", async () => {
    const code = `
    DATA string_to_work_on TYPE string.
    string_to_work_on = \`  \`.
    SHIFT string_to_work_on RIGHT DELETING TRAILING space.
    WRITE strlen( string_to_work_on ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("SHIFT LEFT DELETING LEADING space space", async () => {
    const code = `
  DATA string_to_work_on TYPE string.
  string_to_work_on = \`  \`.
  SHIFT string_to_work_on LEFT DELETING LEADING space.
  ASSERT string_to_work_on = \`\`.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SHIFT right circular", async () => {
    const code = `
    DATA str TYPE string.
    str = '2343.342454332245-'.
    SHIFT str RIGHT CIRCULAR.
    write str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-2343.342454332245");
  });

  it("SHIFT numc", async () => {
    const code = `
    DATA numc TYPE n LENGTH 3.
    DATA foo TYPE string.
    numc = 1.
    SHIFT numc LEFT DELETING LEADING '0'.
    foo = 'hello#world'.
    REPLACE ALL OCCURRENCES OF '#' IN foo WITH numc.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello1world");
  });

  it("SHIFT RIGHT", async () => {
    const code = `
DATA my_variable TYPE c LENGTH 5.
my_variable = 'hello'.
SHIFT my_variable RIGHT BY 3 PLACES.
WRITE / my_variable.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("   he");
  });

});
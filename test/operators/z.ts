import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Bit compare Z", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("test", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF01'.
    hex2 = '11'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no");
  });

  it("Z: operand1 00 operand2 FF - all masked zeros", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = '00'.
    hex2 = 'FF'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("Z: both FF - all masked bits are ones", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF'.
    hex2 = 'FF'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no");
  });

  it("Z: partial match F0 vs FF", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'F0'.
    hex2 = 'FF'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no");
  });

  it("Z: no overlap - F0 vs 0F", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'F0'.
    hex2 = '0F'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("Z: mask all zeros", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF'.
    hex2 = '00'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("Z: operand1 shorter than operand2", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = '00'.
    hex2 = 'FFFF'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("Z: operand1 longer, extra bits ignored", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = '00FF'.
    hex2 = 'FF'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("Z: operand1 shorter, extra mask bits are zeros", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = '00'.
    hex2 = 'FF00'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("Z: with hex type", async () => {
    const code = `
    DATA hex1 TYPE x LENGTH 2.
    DATA hex2 TYPE x LENGTH 2.
    hex1 = '00F0'.
    hex2 = '0F0F'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("NOT Z: negation", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = '00'.
    hex2 = 'FF'.
    IF NOT hex1 Z hex2.
      WRITE 'not-z'.
    ELSE.
      WRITE 'z'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("z");
  });

  it("NOT Z: negation true case", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF'.
    hex2 = 'FF'.
    IF NOT hex1 Z hex2.
      WRITE 'not-z'.
    ELSE.
      WRITE 'z'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("not-z");
  });

  it("Z: single bit", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = '7F'.
    hex2 = '80'.
    IF hex1 Z hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

});
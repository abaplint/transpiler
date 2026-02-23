import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Bit compare O", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("test", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF01'.
    hex2 = '11'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("O: both FF - all bits match", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF'.
    hex2 = 'FF'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("O: operand1 00 operand2 FF - no bits match", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = '00'.
    hex2 = 'FF'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no");
  });

  it("O: partial match F0 vs FF", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'F0'.
    hex2 = 'FF'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no");
  });

  it("O: mask all zeros", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF'.
    hex2 = '00'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("O: operand1 shorter than operand2", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF'.
    hex2 = 'FFFF'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no");
  });

  it("O: operand1 longer, extra bits ignored", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF00'.
    hex2 = 'FF'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("O: operand1 shorter, but extra mask bits are zeros", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF'.
    hex2 = 'FF00'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("O: with hex type", async () => {
    const code = `
    DATA hex1 TYPE x LENGTH 2.
    DATA hex2 TYPE x LENGTH 2.
    hex1 = 'FF11'.
    hex2 = '0011'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("O: no overlap at all", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'F0'.
    hex2 = '0F'.
    IF hex1 O hex2.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no");
  });

  it("NOT O: negation", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = 'FF'.
    hex2 = 'FF'.
    IF NOT hex1 O hex2.
      WRITE 'not-o'.
    ELSE.
      WRITE 'o'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("o");
  });

  it("NOT O: negation false case", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = '00'.
    hex2 = 'FF'.
    IF NOT hex1 O hex2.
      WRITE 'not-o'.
    ELSE.
      WRITE 'o'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("not-o");
  });

  it("O: single bit set", async () => {
    const code = `
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.
    hex1 = '80'.
    hex2 = '80'.
    IF hex1 O hex2.
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
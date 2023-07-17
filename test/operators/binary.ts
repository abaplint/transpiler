import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Bit", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Bit operator BIT-AND", async () => {
    const code = `
    DATA x1 TYPE xstring.
    DATA x2 TYPE xstring.
    DATA x3 TYPE xstring.
    x1 = 'DCBA98765432'.
    x2 = 'DDBBAA885555'.
    x3 = x1 BIT-AND x2.
    WRITE x3.
    x1 = 'DCBA9876543299ABC'.
    x2 = 'DDBBAA885FFFE2'.
    x3 = x1 BIT-AND x2.
    WRITE / x3.
    x1 = 'ABCD'.
    x2 = 'CD'.
    x3 = x1 BIT-AND x2.
    WRITE / x3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("DCBA88005410\nDCBA88005432800000\n8900");
  });

  it("Bit operator BIT-NOT", async () => {
    const code = `
    DATA x1 TYPE xstring.
    DATA x2 TYPE xstring.
    x1 = '0DBD'.
    x2 = BIT-NOT x1.
    WRITE x2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("F242");
  });

  it("Bit operator BIT-OR", async () => {
    const code = `
    DATA x1 TYPE xstring.
    DATA x2 TYPE xstring.
    DATA x3 TYPE xstring.
    x1 = 'DCBA98765432'.
    x2 = 'DDBBAA885555'.
    x3 = x1 BIT-OR x2.
    WRITE x3.
    x1 = 'DCBA9876543299ABC'.
    x2 = 'DDBBAA885FFFE2'.
    x3 = x1 BIT-OR x2.
    WRITE / x3.
    x1 = 'ABCD'.
    x2 = 'CD'.
    x3 = x1 BIT-OR x2.
    WRITE / x3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("DDBBBAFE5577\nDDBBBAFE5FFFFBABC0\nEFCD");
  });

  it("Bit operator BIT-XOR", async () => {
    const code = `
    DATA x1 TYPE xstring.
    DATA x2 TYPE xstring.
    DATA x3 TYPE xstring.
    x1 = 'DCBA98765432'.
    x2 = 'DDBBAA885555'.
    x3 = x1 BIT-XOR x2.
    WRITE x3.
    x1 = 'DCBA9876543299ABC'.
    x2 = 'DDBBAA885FFFE2'.
    x3 = x1 BIT-XOR x2.
    WRITE / x3.
    x1 = 'ABCD'.
    x2 = 'CD'.
    x3 = x1 BIT-XOR x2.
    WRITE / x3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("010132FE0167\n010132FE0BCD7BABC0\n66CD");
  });

  it("Bit operator BIT-XOR, 2", async () => {
    const code = `
    DATA iv_x TYPE x LENGTH 4.
    iv_x = '6F952B2E'.
    DATA iv_y TYPE x LENGTH 4.
    iv_y = 'B49A4DAE'.
    DATA iv_z TYPE x LENGTH 4.
    iv_z = '0A15C329'.
    DATA rv_result TYPE x LENGTH 4.
    rv_result = ( iv_x BIT-XOR iv_y ) BIT-XOR iv_z.
    WRITE rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("D11AA5A9");
  });

  it("Bit operators, 1", async () => {
    const code = `
  DATA iv_x TYPE x LENGTH 4.
  iv_x = 'EFCDAB89'.
  DATA iv_y TYPE x LENGTH 4.
  iv_y = '98BADCFE'.
  DATA iv_z TYPE x LENGTH 4.
  iv_z = '10325476'.
  DATA rv_result TYPE x LENGTH 4.
  rv_result = ( iv_x BIT-AND iv_y ) BIT-OR ( ( BIT-NOT iv_x ) BIT-AND iv_z ).
  WRITE rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("98BADCFE");
  });

  it("Bit operators, 2", async () => {
    const code = `
  DATA iv_x TYPE x LENGTH 4.
  iv_x = 'EFCDAB89'.
  DATA iv_y TYPE x LENGTH 4.
  iv_y = '98BADCFE'.
  DATA iv_z TYPE x LENGTH 4.
  iv_z = '10325476'.
  DATA rv_result TYPE x LENGTH 4.
  rv_result = ( iv_x BIT-AND iv_y ) BIT-OR ( iv_x BIT-AND iv_z ).
  WRITE rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("88888888");
  });

  it("Bit operators, 3", async () => {
    const code = `
  DATA iv_x TYPE x LENGTH 4.
  iv_x = 'EFCDAB89'.
  DATA rv_result TYPE x LENGTH 4.
  rv_result = BIT-NOT iv_x.
  WRITE rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10325476");
  });

  it("Bit operators, xor", async () => {
    const code = `
DATA val1 TYPE xstring.
DATA val2 TYPE xstring.
DATA val3 TYPE xstring.
val1 = '6BC10E0E0E0E0E0E0E0E0E0E0E0E0E0E'.
val2 = '000102030405060708090A0B0C0D0E0F'.
val3 = val1 BIT-XOR val2.
ASSERT val3 = '6BC00C0D0A0B08090607040502030001'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
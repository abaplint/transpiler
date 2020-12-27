import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Bit", () => {

  beforeEach(async () => {
    abap = new ABAP();
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
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("DCBA88005410\nDCBA88005432800000\n8900");
  });

  it.skip("Bit operator BIT-NOT", async () => {
    const code = `
    DATA x1 TYPE xstring.
    DATA x2 TYPE xstring.
    x1 = '0DBD'.
    x2 = BIT-NOT x1.
    WRITE x2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
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
    const f = new Function("abap", js);
    f(abap);
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
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("010132FE0167\n010132FE0BCD7BABC0\n66CD");
  });

});
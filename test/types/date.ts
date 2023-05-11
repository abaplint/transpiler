import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Date type", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("Date initial value", async () => {
    const code = `
      DATA date TYPE d.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00000000");
  });

  it("date initial value", async () => {
    const code = `
      DATA date TYPE d.
      ASSERT date IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Date, assignment from character type", async () => {
    const code = `
      DATA date TYPE d.
      date = '20010203'.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20010203");
  });

  it("Date, assignment from numeric type", async () => {
    const code = `
      DATA date TYPE d.
      DO 24 TIMES.
        date = ( sy-index - 1 ) * 65432 + 1234.
        WRITE / date.
      ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00040519\n01830711\n03620901\n05411023\n07201214\n09000205\n10790329\n12580520\n14370711\n16160911\n17951104\n19741228\n21540219\n23330414\n25120606\n26910730\n28700921\n30491114\n32290106\n34080301\n35870424\n37660616\n39450809\n41241001");
  });

  it("Date, assignment from numeric special cases", async () => {
    const code = `
      DATA date TYPE d.
      date = 577736.
      WRITE date.
      date = 577737.
      WRITE / date.
      date = 0.
      WRITE / date.
      date = 3652060.
      WRITE / date.
      date = 3652061.
      WRITE / date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("15821004\n15821015\n00000000\n99991231\n00000000");
  });

  it("Date, adding 1", async () => {
    const code = `
      DATA date TYPE d.
      date = '00020401'.
      date = date + 1.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00020402");
  });

  it("Date, adding 397", async () => {
    const code = `
      DATA date TYPE d.
      date = '20090807'.
      date = date + 397.
      WRITE date.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20100908");
  });

  it("sy date is set", async () => {
    const code = `WRITE sy-datum.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.not.equal("00000000");
  });

  it("compare empty", async () => {
    const code = `
    DATA var1 TYPE d.
    var1 = ''.
    ASSERT var1 = ''.
    ASSERT var1 = ' '.
    ASSERT var1 = '   '.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - from_mixed", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("test1", async () => {
    const code = `
WRITE / from_mixed( 'putData' ).
`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`PUT_DATA`);
  });

  it("more testing", async () => {
    const code = `
WRITE / from_mixed( 'PutData' ).
WRITE / from_mixed( 'PUTDATA' ).
WRITE / from_mixed( 'putdata' ).
WRITE / from_mixed( 'put data' ).
WRITE / from_mixed( 'put Data' ).
WRITE / from_mixed( '_PutData' ).
`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`PUT_DATA
P_U_T_D_A_T_A
PUTDATA
PUT DATA
PUT _DATA
__PUT_DATA`);
  });

});

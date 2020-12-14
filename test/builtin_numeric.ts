import {expect} from "chai";
import {ABAP} from "../packages/runtime/src/";
import {runFiles} from "./_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin Numeric Functions", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Builtin numerical: abs", async () => {
    const code = `
  DATA int TYPE i.
  int = -3.
  WRITE / int.
  WRITE / abs( int ).
  int = abs( int ).
  WRITE / int.

  WRITE / abs( '-12' ).
  WRITE / abs( -18 ).
  WRITE / abs( 7 ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("-3\n3\n3\n12\n18\n7");
  });

  it("Builtin numerical: ceil", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10. 
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / ceil( chars ).

  packed = chars.
  WRITE / ceil( packed ).

  int = 12.
  WRITE / ceil( int ).

  WRITE / ceil( '43.21' ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("13\n13\n12\n44");
  });

  it("Builtin numerical: floor", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10. 
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / floor( chars ).

  packed = chars.
  WRITE / floor( packed ).

  int = 12.
  WRITE / floor( int ).

  WRITE / floor( '43.21' ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("12\n12\n12\n43");
  });

  it("Builtin numerical: frac", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10. 
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / frac( chars ).

  packed = chars.
  WRITE / frac( packed ).

  int = 12.
  WRITE / frac( int ).

  WRITE / frac( '-43.21' ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0.34\n0.34\n0\n-0.21");
  });

  it("Builtin numerical: sign", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10. 
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / sign( chars ).

  packed = chars.
  WRITE / sign( packed ).

  int = -12.
  WRITE / sign( int ).

  WRITE / frac( '0' ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1\n1\n-1\n0");
  });

  it("Builtin numerical: trunc", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10. 
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / trunc( chars ).

  packed = chars.
  WRITE / trunc( packed ).

  int = 12.
  WRITE / trunc( int ).

  WRITE / trunc( '-43.21' ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("12\n12\n12\n-43");
  });
});

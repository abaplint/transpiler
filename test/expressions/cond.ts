import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_cond.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - COND", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
FORM foo.
  DATA(url) = COND string( WHEN 1 = 2 THEN 'foo'
                           WHEN 1 = 1 THEN 'bar' ).
  WRITE / url.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("else", async () => {
    const code = `
FORM foo.
  DATA(url) = COND string( WHEN 1 = 2 THEN 'foo'
                           ELSE 'bar' ).
  WRITE / url.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("no true condition", async () => {
    const code = `
FORM foo.
  DATA(url) = COND string( WHEN 1 = 2 THEN 'foo'
                           WHEN 1 = 2 THEN 'bar' ).
  WRITE / url.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("lower case ELSE", async () => {
    const code = `
FORM foo.
  DATA include_tax TYPE abap_bool.
  DATA lv_net TYPE i.
  DATA lv_tax_rate TYPE i.
  DATA(lv_tax) = COND decfloat34(
    WHEN include_tax = abap_true
    THEN lv_net * lv_tax_rate
    else 0 ).
  WRITE / lv_tax.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

});
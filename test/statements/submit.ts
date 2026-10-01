import {expect} from "chai";
import {ABAP, MemoryConsole, ProgramRegistry} from "../../packages/runtime/src";
import {Transpiler} from "../../packages/transpiler/src";
import {AsyncFunction} from "../_utils";

let abap: ABAP;

/** transpiles every program, registers all but the first with the SUBMIT host,
 * runs the first and returns the console */
async function run(...programs: string[]) {
  const files = programs.map(p => {
    const name = p.match(/REPORT (\w+)\./i)![1].toLowerCase();
    return {filename: name + ".prog.abap", contents: p};
  });
  const res = await new Transpiler().runRaw(files);
  const registry = new ProgramRegistry();
  abap = new ABAP({console: new MemoryConsole(), submit: registry});
  (global as any).abap = abap;
  for (const o of res.objects) {
    const code = o.chunk.getCode();
    registry.add(o.object.name, async () => new AsyncFunction("abap", code)(abap));
  }
  await registry.submit({program: files[0].filename.split(".")[0].toUpperCase(), selections: []});
  return abap.console.get();
}

const callee = `REPORT zcallee.
DATA gv TYPE c LENGTH 10.
PARAMETERS p_txt(4) DEFAULT 'def'.
PARAMETERS p_low(4) LOWER CASE.
PARAMETERS p_keep(4) DEFAULT 'keep'.
PARAMETERS p_num TYPE i DEFAULT 7.
SELECT-OPTIONS s_a FOR gv DEFAULT 'A1'.
SELECT-OPTIONS s_b FOR gv.
START-OF-SELECTION.
  DATA lv TYPE string.
  DATA ls LIKE LINE OF s_a.
  lv = |txt=<{ p_txt }> low=<{ p_low }> keep=<{ p_keep }> num={ p_num } s_a={ lines( s_a ) }|.
  LOOP AT s_a INTO ls.
    lv = |{ lv } [{ ls-sign }{ ls-option }:{ ls-low }:{ ls-high }]|.
  ENDLOOP.
  lv = |{ lv } s_b={ lines( s_b ) }|.
  LOOP AT s_b INTO ls.
    lv = |{ lv } [{ ls-sign }{ ls-option }:{ ls-low }:{ ls-high }]|.
  ENDLOOP.
  WRITE / lv.`;

// expectations measured on a 7.5x system, SUBMIT ... AND RETURN from a report
// running as a background step; the callee above is the one measured with
describe("Running statements - SUBMIT", () => {

  it("no WITH, the defaults", async () => {
    const caller = `REPORT zcaller.
    SUBMIT zcallee AND RETURN.`;
    expect(await run(caller, callee)).to.equal(
      "txt=<DEF> low=<> keep=<KEEP> num=7 s_a=1 [IEQ:A1:] s_b=0");
  });

  it("parameters upper-cased unless LOWER CASE, a select-option's WITH replaces its default", async () => {
    const caller = `REPORT zcaller.
    SUBMIT zcallee WITH p_txt = 'abc' WITH p_low = 'abc' WITH s_a = 'x1' AND RETURN.`;
    expect(await run(caller, callee)).to.equal(
      "txt=<ABC> low=<abc> keep=<KEEP> num=7 s_a=1 [IEQ:X1:] s_b=0");
  });

  it("BETWEEN, SIGN and options; only the first row is upper-cased", async () => {
    const caller = `REPORT zcaller.
    SUBMIT zcallee WITH s_a BETWEEN 'a' AND 'c' WITH s_b NE 'q' SIGN 'E' WITH s_b CP 'z*' AND RETURN.`;
    expect(await run(caller, callee)).to.equal(
      "txt=<DEF> low=<> keep=<KEEP> num=7 s_a=1 [IBT:A:C] s_b=2 [ENE:Q:] [ICP:z*:]");
  });

  it("IN, the rows of a range table", async () => {
    const caller = `REPORT zcaller.
    DATA gv TYPE c LENGTH 10.
    DATA lr LIKE RANGE OF gv.
    lr = VALUE #( ( sign = 'I' option = 'EQ' low = 'lo1' ) ( sign = 'E' option = 'CP' low = 'lo*' ) ).
    SUBMIT zcallee WITH s_a IN lr AND RETURN.`;
    expect(await run(caller, callee)).to.equal(
      "txt=<DEF> low=<> keep=<KEEP> num=7 s_a=2 [IEQ:LO1:] [ECP:lo*:] s_b=0");
  });

  it("a value converts, an initial value replaces a default", async () => {
    const caller = `REPORT zcaller.
    SUBMIT zcallee WITH p_num = '12' WITH p_keep = '' AND RETURN.`;
    expect(await run(caller, callee)).to.equal(
      "txt=<DEF> low=<> keep=<> num=12 s_a=1 [IEQ:A1:] s_b=0");
  });

  it("several WITH for one select-option append in order, the second keeps its case", async () => {
    const caller = `REPORT zcaller.
    DATA gv TYPE c LENGTH 10.
    DATA lr LIKE RANGE OF gv.
    lr = VALUE #( ( sign = 'I' option = 'EQ' low = 'lo1' ) ( sign = 'E' option = 'CP' low = 'lo*' ) ).
    SUBMIT zcallee WITH s_a IN lr WITH s_a = 'y1' AND RETURN.
    SUBMIT zcallee WITH s_b = 'q1' WITH s_b = 'q2' WITH s_b = 'q3' AND RETURN.
    SUBMIT zcallee WITH s_b CP 'z*' AND RETURN.
    SUBMIT zcallee WITH s_b GE 'g1' WITH s_b LT 'l1' AND RETURN.`;
    expect(await run(caller, callee)).to.equal(
      "txt=<DEF> low=<> keep=<KEEP> num=7 s_a=3 [IEQ:LO1:] [ECP:lo*:] [IEQ:y1:] s_b=0" +
      "\ntxt=<DEF> low=<> keep=<KEEP> num=7 s_a=1 [IEQ:A1:] s_b=3 [IEQ:Q1:] [IEQ:q2:] [IEQ:q3:]" +
      "\ntxt=<DEF> low=<> keep=<KEEP> num=7 s_a=1 [IEQ:A1:] s_b=1 [ICP:Z*:]" +
      "\ntxt=<DEF> low=<> keep=<KEEP> num=7 s_a=1 [IEQ:A1:] s_b=2 [IGE:G1:] [ILT:l1:]");
  });

  it("sy-subrc of the caller is kept, LEAVE PROGRAM returns to it", async () => {
    const leaver = `REPORT zleaver.
    PARAMETERS p_mode(1).
    START-OF-SELECTION.
      CASE p_mode.
        WHEN 'L'.
          LEAVE PROGRAM.
        WHEN 'S'.
          sy-subrc = 7.
          LEAVE PROGRAM.
        WHEN 'Z'.
          sy-subrc = 0.
      ENDCASE.
      WRITE / 'end'.`;
    const caller = `REPORT zcaller.
    DATA lt_modes TYPE STANDARD TABLE OF c WITH DEFAULT KEY.
    DATA lv_mode TYPE c LENGTH 1.
    APPEND 'L' TO lt_modes.
    APPEND 'S' TO lt_modes.
    APPEND 'Z' TO lt_modes.
    APPEND 'N' TO lt_modes.
    LOOP AT lt_modes INTO lv_mode.
      sy-subrc = 3.
      SUBMIT zleaver WITH p_mode = lv_mode AND RETURN.
      WRITE / |{ lv_mode }:{ sy-subrc }|.
    ENDLOOP.`;
    expect(await run(caller, leaver)).to.equal("L:3\nS:3\nend\nZ:3\nend\nN:3");
  });

  it("every SUBMIT starts the program from its declarations", async () => {
    const caller = `REPORT zcaller.
    SUBMIT zcallee WITH p_num = 1 AND RETURN.
    SUBMIT zcallee AND RETURN.`;
    expect(await run(caller, callee)).to.equal(
      "txt=<DEF> low=<> keep=<KEEP> num=1 s_a=1 [IEQ:A1:] s_b=0" +
      "\ntxt=<DEF> low=<> keep=<KEEP> num=7 s_a=1 [IEQ:A1:] s_b=0");
  });

  it("a value that does not convert is a runtime error, not an ABAP exception", async () => {
    const caller = `REPORT zcaller.
    SUBMIT zcallee WITH p_num = 'abc' AND RETURN.
    WRITE / 'after'.`;
    let error: any;
    try {
      await run(caller, callee);
    } catch (e: any) {
      error = e;
    }
    expect(error?.constructor?.name).to.equal("Error");
    expect(error?.message).to.contain("P_NUM");
    expect(abap.console.get()).to.equal("");
  });

  it("an exception the program does not handle is a runtime error, not an ABAP exception", async () => {
    const caller = `REPORT zcaller.
    SUBMIT zthrows AND RETURN.
    WRITE / 'after'.`;
    const res = await new Transpiler().runRaw([{filename: "zcaller.prog.abap", contents: caller}]);
    const registry = new ProgramRegistry();
    abap = new ABAP({console: new MemoryConsole(), submit: registry});
    (global as any).abap = abap;
    // what RAISE EXCEPTION throws: an instance of an ABAP class, not a javascript Error
    class CxSyZerodivide {}
    registry.add("ZTHROWS", async () => { throw new CxSyZerodivide(); });
    let error: any;
    try {
      await new AsyncFunction("abap", res.objects[0].chunk.getCode())(abap);
    } catch (e: any) {
      error = e;
    }
    expect(error?.constructor?.name).to.equal("Error");
    expect(error?.message).to.contain("ZTHROWS");
    expect(abap.console.get()).to.equal("");
  });

  it("a program the host does not know", async () => {
    const caller = `REPORT zcaller.
    DATA lv TYPE c LENGTH 10 VALUE 'znothere'.
    SUBMIT (lv) AND RETURN.`;
    let message = "";
    try {
      await run(caller);
    } catch (e: any) {
      message = e.message;
    }
    expect(message).to.contain("ZNOTHERE");
  });

  it("without AND RETURN is not supported", async () => {
    const caller = `REPORT zcaller.
    SUBMIT zcallee.`;
    let message = "";
    try {
      await run(caller, callee);
    } catch (e: any) {
      message = e.message;
    }
    expect(message).to.contain("without AND RETURN");
  });

});

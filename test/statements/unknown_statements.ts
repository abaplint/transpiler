import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {UnknownTypesEnum} from "../../packages/transpiler/src/types";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

const code = `
DATA a TYPE i.
DATA b TYPE i.
FORM other.
  WRITE / 'other'.
ENDFORM.
START-OF-SELECTION.
  WRITE / 'before'.
  IF a = 1.
    ADD-CORRESPONDING a TO b.
  ENDIF.
  PERFORM other.
  ADD-CORRESPONDING a TO b.
  WRITE / 'after'.`;

describe("Statements that cannot be transpiled", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("fail the transpilation by default", async () => {
    let message = "";
    try {
      await runFiles(abap, [{filename: "zfoobar_unknown.prog.abap", contents: code}], {skipVersionCheck: true});
    } catch (e) {
      message = String(e);
    }
    expect(message).to.include("not supported");
  });

  it("with unknownStatements = runtimeError, fail only when they run", async () => {
    const js = await runFiles(abap, [{filename: "zfoobar_unknown.prog.abap", contents: code}],
                              {skipVersionCheck: true, unknownStatements: UnknownTypesEnum.runtimeError});
    const f = new AsyncFunction("abap", js);
    let message = "";
    try {
      await f(abap);
    } catch (e) {
      message = String(e);
    }
    expect(abap.console.get()).to.equal("before\nother");
    expect(message).to.include("ADD-CORRESPONDING, not supported, transpiler");
  });

  it("with unknownStatements = runtimeError, a block that cannot be transpiled fails when it runs", async () => {
    const provide = `
TYPES: BEGIN OF ty,
         lo  TYPE i,
         hi  TYPE i,
         txt TYPE c LENGTH 1,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row TYPE ty.
DATA flag TYPE c LENGTH 1.
WRITE / 'before'.
PROVIDE FIELDS txt FROM tab INTO row VALID flag BOUNDS lo AND hi BETWEEN 1 AND 5.
  WRITE / row-txt.
ENDPROVIDE.
WRITE / 'after'.`;
    const js = await runFiles(abap, [{filename: "zfoobar_unknown.prog.abap", contents: provide}],
                              {skipVersionCheck: true, unknownStatements: UnknownTypesEnum.runtimeError});
    const f = new AsyncFunction("abap", js);
    let message = "";
    try {
      await f(abap);
    } catch (e) {
      message = String(e);
    }
    expect(abap.console.get()).to.equal("before");
    expect(message).to.include("PROVIDE FIELDS, not supported, transpiler");
  });

});

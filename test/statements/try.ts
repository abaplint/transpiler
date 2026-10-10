import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {expect} from "chai";
import {AsyncFunction, runFiles} from "../_utils";
import {UnknownTypesEnum} from "../../packages/transpiler/src/types";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}], {unknownTypes: UnknownTypesEnum.runtimeError});
}

const exceptions = `
CLASS cx_root DEFINITION ABSTRACT PUBLIC.
  PUBLIC SECTION.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.

CLASS cx_static_check DEFINITION PUBLIC INHERITING FROM cx_root ABSTRACT.
ENDCLASS.
CLASS cx_static_check IMPLEMENTATION.
ENDCLASS.

CLASS lcx_first DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS lcx_first IMPLEMENTATION.
ENDCLASS.

CLASS lcx_second DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS lcx_second IMPLEMENTATION.
ENDCLASS.`;

describe("Running statements - TRY", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("TRY without CATCH", async () => {
    const code = `
      TRY.
        WRITE 'hello'.
      ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("TRY, non existing class", async () => {
    const code = `
      TRY.
        WRITE '@KERNEL throw "hello";'.
      CATCH cx_not_existing.
      ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
    } catch (e) {
      expect(e + "").to.include("hello");
    }
  });

  it("CLEANUP, exception not caught by the own CATCH", async () => {
    const code = `
${exceptions}

START-OF-SELECTION.
  TRY.
      TRY.
          RAISE EXCEPTION TYPE lcx_first.
        CATCH lcx_second.
          WRITE / 'own catch'.
        CLEANUP.
          WRITE / 'cleanup'.
      ENDTRY.
    CATCH lcx_first.
      WRITE / 'outer catch'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("cleanup\nouter catch");
  });

  it("CLEANUP, exception caught by the own CATCH", async () => {
    const code = `
${exceptions}

START-OF-SELECTION.
  TRY.
      TRY.
          RAISE EXCEPTION TYPE lcx_first.
        CATCH lcx_first.
          WRITE / 'own catch'.
        CLEANUP.
          WRITE / 'cleanup'.
      ENDTRY.
    CATCH lcx_first.
      WRITE / 'outer catch'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("own catch");
  });

  it("CLEANUP, exception raised in the own CATCH", async () => {
    const code = `
${exceptions}

START-OF-SELECTION.
  TRY.
      TRY.
          RAISE EXCEPTION TYPE lcx_first.
        CATCH lcx_first.
          WRITE / 'own catch'.
          RAISE EXCEPTION TYPE lcx_second.
        CLEANUP.
          WRITE / 'cleanup'.
      ENDTRY.
    CATCH lcx_second.
      WRITE / 'outer catch'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("own catch\nouter catch");
  });

  it("CLEANUP, two TRY, inner CLEANUP first", async () => {
    const code = `
${exceptions}

START-OF-SELECTION.
  TRY.
      TRY.
          TRY.
              RAISE EXCEPTION TYPE lcx_first.
            CLEANUP.
              WRITE / 'inner cleanup'.
          ENDTRY.
        CLEANUP.
          WRITE / 'outer cleanup'.
      ENDTRY.
    CATCH lcx_first.
      WRITE / 'catch'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("inner cleanup\nouter cleanup\ncatch");
  });

  it("CLEANUP INTO", async () => {
    const code = `
${exceptions}

START-OF-SELECTION.
  DATA raised TYPE REF TO lcx_first.
  DATA cleaned TYPE REF TO cx_root.
  CREATE OBJECT raised.
  TRY.
      TRY.
          RAISE EXCEPTION raised.
        CLEANUP INTO cleaned.
          IF cleaned = raised.
            WRITE / 'cleanup, same object'.
          ENDIF.
      ENDTRY.
    CATCH lcx_first.
      WRITE / 'catch'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("cleanup, same object\ncatch");
  });

  it("CLEANUP in a called method", async () => {
    const code = `
${exceptions}

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run RAISING lcx_first.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    TRY.
        RAISE EXCEPTION TYPE lcx_first.
      CLEANUP.
        WRITE / 'method cleanup'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TRY.
      lcl=>run( ).
    CATCH lcx_first.
      WRITE / 'caller catch'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("method cleanup\ncaller catch");
  });

  it("CLEANUP, not for RETURN", async () => {
    const code = `
${exceptions}

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    TRY.
        WRITE / 'try'.
        RETURN.
      CLEANUP.
        WRITE / 'cleanup'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("try");
  });

  it("CLEANUP, not for a javascript error", async () => {
    const code = `
${exceptions}

START-OF-SELECTION.
  TRY.
      WRITE '@KERNEL throw "hello";'.
    CLEANUP.
      WRITE / 'cleanup'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e + "").to.include("hello");
    }
    expect(abap.console.get()).to.equal("");
  });

});

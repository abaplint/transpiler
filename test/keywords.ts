import {expect} from "chai";
import {ABAP, MemoryConsole} from "../packages/runtime/src/";
import {AsyncFunction, runFiles} from "./_utils";
import {ITranspilerOptions} from "../packages/transpiler/src";

let abap: ABAP;

async function run(contents: string) {
  const options: ITranspilerOptions = {
    keywords: [],
  };
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}], options);
}

describe("Keywords", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("class", async () => {
    const code = `
DATA class TYPE x.
class = 'AA'.
WRITE class.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("AA");
  });

  it("class, case", async () => {
    const code = `
DATA CLASS TYPE x.
class = 'AA'.
WRITE class.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("AA");
  });

  it("class, INITIAL", async () => {
    const code = `
DATA CLASS TYPE x.
IF class IS INITIAL.
ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("method parameter", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING class TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE / class.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( 2 ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("method parameter, returning", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo RETURNING VALUE(class) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    class = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("form parameter", async () => {
    const code = `
FORM foo USING class TYPE i.
  WRITE class.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo USING 2.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("class attribute", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
  PRIVATE SECTION.
    DATA class TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    WRITE class.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("with, case", async () => {
    // both are reserved in JavaScript and legal as ABAP names, so the emitted
    // `let with = ...` is a SyntaxError rather than a wrong value
    const code = `
DATA with TYPE i.
DATA case TYPE i.
with = 1.
case = 2.
WRITE with.
WRITE case.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12");
  });

  // "abap" is not a JavaScript reserved word, but the emitted code reaches the
  // runtime through a free identifier of that name: a `let abap` at program
  // level is "Identifier 'abap' has already been declared", inside a method
  // every `abap.types...` above the declaration is in the temporal dead zone
  it("abap, DATA at program level", async () => {
    const code = `
DATA x TYPE i.
x = 1.
DATA abap TYPE i.
abap = x + 1.
WRITE abap.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("abap, local DATA in a method, declared after a runtime access", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    DATA x TYPE i.
    x = 1.
    DATA(abap) = x + 1.
    WRITE abap.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("abap, method parameters, importing and exporting", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING abap TYPE i RETURNING VALUE(r) TYPE i.
    CLASS-METHODS bar EXPORTING abap TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    r = abap + 1.
  ENDMETHOD.
  METHOD bar.
    abap = 7.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA v TYPE i.
  v = lcl=>foo( abap = 1 ).
  WRITE v.
  lcl=>bar( IMPORTING abap = v ).
  WRITE v.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("27");
  });

  it("abap, field symbol", async () => {
    const code = `
DATA v TYPE i.
FIELD-SYMBOLS <abap> TYPE i.
ASSIGN v TO <abap>.
<abap> = 5.
WRITE v.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("abap, class attribute and method name", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA abap TYPE i.
    METHODS set IMPORTING val TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD set.
    me->abap = val.
  ENDMETHOD.
ENDCLASS.

CLASS lcl2 DEFINITION.
  PUBLIC SECTION.
    METHODS abap RETURNING VALUE(r) TYPE i.
ENDCLASS.

CLASS lcl2 IMPLEMENTATION.
  METHOD abap.
    r = 9.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  DATA lo2 TYPE REF TO lcl2.
  CREATE OBJECT lo.
  CREATE OBJECT lo2.
  lo->set( 3 ).
  WRITE lo->abap.
  WRITE lo2->abap( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("39");
  });

});

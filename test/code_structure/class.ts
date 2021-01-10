import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running code structure - Class", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Class, simple method call", async () => {
    const code = `
      CLASS zcl_words DEFINITION.
        PUBLIC SECTION.
          METHODS
            run.
      ENDCLASS.

      CLASS zcl_words IMPLEMENTATION.
        METHOD run.
          WRITE 'foo'.
        ENDMETHOD.
      ENDCLASS.

      DATA foo TYPE REF TO zcl_words.
      CREATE OBJECT foo.
      foo->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("Class, call method in same class", async () => {
    const code = `
      CLASS zcl_words DEFINITION.
        PUBLIC SECTION.
          METHODS:
            run,
            bar.
      ENDCLASS.

      CLASS zcl_words IMPLEMENTATION.
        METHOD run.
          bar( ).
        ENDMETHOD.
        METHOD bar.
          WRITE 'foo'.
        ENDMETHOD.
      ENDCLASS.

      DATA foo TYPE REF TO zcl_words.
      CREATE OBJECT foo.
      foo->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("Class, attribute", async () => {
    const code = `
      CLASS zcl_words DEFINITION.
        PUBLIC SECTION.
          DATA bar TYPE i.
          METHODS: run.
      ENDCLASS.

      CLASS zcl_words IMPLEMENTATION.
        METHOD run.
          WRITE bar.
        ENDMETHOD.
      ENDCLASS.

      DATA foo TYPE REF TO zcl_words.
      CREATE OBJECT foo.
      foo->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("Class, constructor", async () => {
    const code = `
      CLASS zcl_words DEFINITION.
        PUBLIC SECTION.
          DATA bar TYPE i.
          METHODS: constructor.
      ENDCLASS.

      CLASS zcl_words IMPLEMENTATION.
        METHOD constructor.
          bar = 2.
          WRITE bar.
        ENDMETHOD.
      ENDCLASS.

      DATA foo TYPE REF TO zcl_words.
      CREATE OBJECT foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("CLASS-DATA", async () => {
    const code = `
      CLASS zcl_words DEFINITION.
        PUBLIC SECTION.
          CLASS-DATA bar TYPE i.
          METHODS: run.
      ENDCLASS.

      CLASS zcl_words IMPLEMENTATION.
        METHOD run.
          bar = 2.
        ENDMETHOD.
      ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CLASS-DATA, type ref", async () => {
    const code = `
      CLASS zcl_words DEFINITION.
        PUBLIC SECTION.
          CLASS-DATA bar TYPE REF TO zcl_words.
          METHODS: run.
      ENDCLASS.

      CLASS zcl_words IMPLEMENTATION.
        METHOD run.
          CREATE OBJECT bar.
        ENDMETHOD.
      ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("static variable in class", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
        PUBLIC SECTION.
          CLASS-DATA foo TYPE i.
          CLASS-METHODS name.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
        METHOD name.
          CLEAR foo.
        ENDMETHOD.
      ENDCLASS.

      lcl_bar=>name( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("early RETURN in method", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
        PUBLIC SECTION.
          CLASS-METHODS bar RETURNING VALUE(ret) TYPE i.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
        METHOD bar.
          ret = 1.
          RETURN.
        ENDMETHOD.
      ENDCLASS.

        WRITE lcl_bar=>bar( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("EXPORTING value", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
        PUBLIC SECTION.
          METHODS bar EXPORTING val TYPE i.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
        METHOD bar.
          val = 2.
        ENDMETHOD.
      ENDCLASS.

      START-OF-SELECTION.
        DATA bar TYPE REF TO lcl_bar.
        CREATE OBJECT bar.
        DATA res TYPE i.
        bar->bar( IMPORTING val = res ).
        WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("class constant from static method", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
        PUBLIC SECTION.
          CONSTANTS c TYPE i VALUE 10.
          CLASS-METHODS foo.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
        METHOD foo.
          WRITE c.
        ENDMETHOD.
      ENDCLASS.

      lcl_bar=>foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("class constant from instance method", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
        PUBLIC SECTION.
          CONSTANTS c TYPE i VALUE 10.
          METHODS foo.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
        METHOD foo.
          WRITE c.
        ENDMETHOD.
      ENDCLASS.

        DATA bar TYPE REF TO lcl_bar.
        CREATE OBJECT bar.
        bar->foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("class, testing me->", async () => {
    const code = `
      CLASS lcl_foo DEFINITION.
        PUBLIC SECTION.
          DATA moo TYPE i.
          METHODS constructor.
      ENDCLASS.

      CLASS lcl_foo IMPLEMENTATION.
        METHOD constructor.
          me->moo = 2.
          WRITE me->moo.
        ENDMETHOD.
      ENDCLASS.

      START-OF-SELECTION.
        DATA bar TYPE REF TO lcl_foo.
        CREATE OBJECT bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("call method in super class, no constructor", async () => {
    const code = `
      CLASS zcl_super DEFINITION.
        PUBLIC SECTION.
          METHODS method.
      ENDCLASS.

      CLASS zcl_super IMPLEMENTATION.
        METHOD method.
          WRITE 4.
        ENDMETHOD.
      ENDCLASS.

      CLASS zcl_sub DEFINITION INHERITING FROM zcl_super.
      ENDCLASS.

      CLASS zcl_sub IMPLEMENTATION.
      ENDCLASS.

      FORM run.
        DATA sub TYPE REF TO zcl_sub.
        CREATE OBJECT sub.
        sub->method( ).
      ENDFORM.

      START-OF-SELECTION.
        PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("call super in redefined method", async () => {
    const code = `
      CLASS zcl_super DEFINITION.
        PUBLIC SECTION.
          METHODS method.
      ENDCLASS.

      CLASS zcl_super IMPLEMENTATION.
        METHOD method.
          WRITE / 'b'.
        ENDMETHOD.
      ENDCLASS.

      CLASS zcl_sub DEFINITION INHERITING FROM zcl_super.
        PUBLIC SECTION.
          METHODS:
            method REDEFINITION.
      ENDCLASS.

      CLASS zcl_sub IMPLEMENTATION.
        METHOD method.
          WRITE / 'a'.
          super->method( ).
        ENDMETHOD.
      ENDCLASS.

      FORM run.
        DATA sub TYPE REF TO zcl_sub.
        CREATE OBJECT sub.
        sub->method( ).
      ENDFORM.

      START-OF-SELECTION.
        PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("a\nb");
  });

  it("testing initialization of variables in constructor", async () => {
    const code = `
      CLASS zcl_super DEFINITION.
        PUBLIC SECTION.
          DATA foo TYPE i.
          METHODS constructor.
      ENDCLASS.

      CLASS zcl_super IMPLEMENTATION.
        METHOD constructor.
          foo = 1.
        ENDMETHOD.
      ENDCLASS.

      CLASS zcl_sub DEFINITION INHERITING FROM zcl_super.
        PUBLIC SECTION.
          METHODS constructor.
      ENDCLASS.

      CLASS zcl_sub IMPLEMENTATION.
        METHOD constructor.
          super->constructor( ).
          WRITE foo.
        ENDMETHOD.
      ENDCLASS.

      START-OF-SELECTION.
        DATA moo TYPE REF TO zcl_sub.
        CREATE OBJECT moo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("test refs are identical", async () => {
    const code = `
      CLASS lcl_foo DEFINITION.
      ENDCLASS.
      CLASS lcl_foo IMPLEMENTATION.
      ENDCLASS.
      DATA ref1 TYPE REF TO lcl_foo.
      DATA ref2 TYPE REF TO lcl_foo.
      CREATE OBJECT ref1.
      ref2 = ref1.
      ASSERT ref1 = ref2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("determine default parameter name", async () => {
    const code = `
      CLASS cl DEFINITION.
        PUBLIC SECTION.
          CLASS-METHODS name
            IMPORTING
              iv_url      TYPE string
              iv_validate TYPE abap_bool DEFAULT abap_false.
      ENDCLASS.
      CLASS cl IMPLEMENTATION.
        METHOD name.
          WRITE iv_url.
        ENDMETHOD.
      ENDCLASS.

      FORM bar.
        cl=>name( 'bar' ).
      ENDFORM.

      PERFORM bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("CREATE OBJECT with dashes/structure", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
      ENDCLASS.
      TYPES: BEGIN OF ty_structure,
              field TYPE REF TO lcl_bar,
            END OF ty_structure.
      FORM moo.
        DATA ls_structure TYPE ty_structure.
        CREATE OBJECT ls_structure-field.
      ENDFORM.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("class constructor", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
        PUBLIC SECTION.
          CLASS-METHODS class_constructor.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
        METHOD class_constructor.
          WRITE 'hello'.
        ENDMETHOD.
      ENDCLASS.

      FORM bar.
        DATA lo_bar TYPE REF TO lcl_bar.
        CREATE OBJECT lo_bar.
      ENDFORM.

      START-OF-SELECTION.
        PERFORM bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("Method with IMPORTING default value", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
        PUBLIC SECTION.
          CLASS-METHODS: moo IMPORTING bar LIKE sy-msgid DEFAULT sy-msgid.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
        METHOD moo.
          WRITE bar.
        ENDMETHOD.
      ENDCLASS.
      FORM form.
        sy-msgid = '123'.
        lcl_bar=>moo( ).
      ENDFORM.
      START-OF-SELECTION.
        PERFORM form.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("CALL METHOD", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
        PUBLIC SECTION.
          METHODS name.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
        METHOD name.
          WRITE / 'hello'.
        ENDMETHOD.
      ENDCLASS.
      START-OF-SELECTION.
        PERFORM bar.
      FORM bar.
        DATA bar TYPE REF TO lcl_bar.
        CREATE OBJECT bar.
        CALL METHOD bar->name( ).
        CALL METHOD bar->name.
      ENDFORM.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\nhello");
  });

  it.skip("CALL METHOD with EXPORTING", async () => {
    const code = `
      CLASS lcl_bar DEFINITION.
        PUBLIC SECTION.
          METHODS name IMPORTING foo TYPE i.
      ENDCLASS.
      CLASS lcl_bar IMPLEMENTATION.
        METHOD name.
          WRITE / foo.
        ENDMETHOD.
      ENDCLASS.
      START-OF-SELECTION.
        PERFORM bar.
      FORM bar.
        DATA bar TYPE REF TO lcl_bar.
        CREATE OBJECT bar.
        CALL METHOD bar->name( 1 ).
        CALL METHOD bar->name( foo = 2 ).
        CALL METHOD bar->name EXPORTING foo = 3.
      ENDFORM.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("call method in constructor", async () => {
    const code = `
CLASS lclas DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS foobar.
ENDCLASS.

CLASS lclas IMPLEMENTATION.
  METHOD constructor.
    foobar( ).
  ENDMETHOD.
  METHOD foobar.
    WRITE 'hello'.
  ENDMETHOD.
ENDCLASS.

FORM bar.
  DATA cl TYPE REF TO lclas.
  CREATE OBJECT cl.
  ASSERT NOT cl IS INITIAL.
ENDFORM.

START-OF-SELECTION.
  PERFORM bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("method call chain", async () => {
    const code = `
CLASS clas DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS bar1 RETURNING VALUE(ref) TYPE REF TO clas.
    CLASS-METHODS bar2 RETURNING VALUE(val) TYPE string.
ENDCLASS.
CLASS clas IMPLEMENTATION.
  METHOD bar1.
    CREATE OBJECT ref.
  ENDMETHOD.
  METHOD bar2.
    val = 'hello'.
  ENDMETHOD.
ENDCLASS.

FORM moo.
  WRITE clas=>bar1( )->bar2( ).
ENDFORM.

START-OF-SELECTION.
  PERFORM moo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("method parameter uppercase", async () => {
    const code = `
      CLASS cl DEFINITION.
        PUBLIC SECTION.
          CLASS-METHODS name
            IMPORTING
              IV_URL TYPE string.
      ENDCLASS.
      CLASS cl IMPLEMENTATION.
        METHOD name.
          WRITE / iv_url.
          WRITE / IV_URL.
          WRITE / iV_URl.
        ENDMETHOD.
      ENDCLASS.

      FORM bar.
        cl=>name( 'bar' ).
      ENDFORM.

      PERFORM bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar\nbar\nbar");
  });

  it("testing initialization of variables in constructor, with CALL METHOD", async () => {
    const code = `
      CLASS zcl_super DEFINITION.
        PUBLIC SECTION.
          DATA foo TYPE i.
          METHODS constructor.
      ENDCLASS.

      CLASS zcl_super IMPLEMENTATION.
        METHOD constructor.
          foo = 1.
        ENDMETHOD.
      ENDCLASS.

      CLASS zcl_sub DEFINITION INHERITING FROM zcl_super.
        PUBLIC SECTION.
          METHODS constructor.
      ENDCLASS.

      CLASS zcl_sub IMPLEMENTATION.
        METHOD constructor.
          CALL METHOD super->constructor( ).
          WRITE foo.
        ENDMETHOD.
      ENDCLASS.

      START-OF-SELECTION.
        DATA moo TYPE REF TO zcl_sub.
        CREATE OBJECT moo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("testing initialization of variables in constructor, with CALL METHOD 2", async () => {
    const code = `
CLASS zcl_super DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE i.
    METHODS constructor.
ENDCLASS.

CLASS zcl_super IMPLEMENTATION.
  METHOD constructor.
    foo = 1.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_sub DEFINITION INHERITING FROM zcl_super.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS zcl_sub IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    WRITE foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA moo TYPE REF TO zcl_sub.
  CREATE OBJECT moo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("attribute case, upper case FOO", async () => {
    const code = `
CLASS zcl_super DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE i.
    METHODS constructor.
ENDCLASS.

CLASS zcl_super IMPLEMENTATION.
  METHOD constructor.
    me->FOO = 1.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA moo TYPE REF TO zcl_super.
  CREATE OBJECT moo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("implement interface, method name is js keyword", async () => {
    const code = `
INTERFACE foo.
  METHODS delete.
ENDINTERFACE.
CLASS bar DEFINITION.
  PUBLIC SECTION.
    INTERFACES foo.
ENDCLASS.
CLASS bar IMPLEMENTATION.
  METHOD foo~delete.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("method name is js keyword", async () => {
    const code = `
CLASS bar DEFINITION.
  PUBLIC SECTION.
    METHODS delete.
ENDCLASS.
CLASS bar IMPLEMENTATION.
  METHOD delete.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
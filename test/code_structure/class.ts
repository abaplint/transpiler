import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, compileFiles, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running code structure - Class", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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

  it("another call super in redefined method", async () => {
    const code = `
CLASS sup DEFINITION.
  PUBLIC SECTION.
    METHODS init.
ENDCLASS.
CLASS sup IMPLEMENTATION.
  METHOD init.
    WRITE / 'hello'.
  ENDMETHOD.
ENDCLASS.

CLASS sub DEFINITION INHERITING FROM sup.
  PUBLIC SECTION.
    METHODS init REDEFINITION.
ENDCLASS.

CLASS sub IMPLEMENTATION.
  METHOD init.
    CALL METHOD super->init.
    WRITE / 'world'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO sub.
  CREATE OBJECT ref.
  ref->init( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\nworld");
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
          CLASS-METHODS moo IMPORTING bar LIKE sy-msgid DEFAULT sy-msgid.
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
    expect(abap.console.get().trimEnd()).to.equal("123");
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

  it("class constructor, set structured var", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    CLASS-DATA: BEGIN OF bar,
                  field TYPE string,
                END OF bar.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD class_constructor.
    bar-field = 'abc'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  WRITE lcl_bar=>bar-field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abc");
  });

  it("class, alias field from interface, static", async () => {
    const code = `
INTERFACE intf.
  CLASS-DATA bar TYPE i.
ENDINTERFACE.

CLASS clas DEFINITION.
  PUBLIC SECTION.
    INTERFACES intf.
    ALIASES bar FOR intf~bar.
    CLASS-METHODS run.
ENDCLASS.

CLASS clas IMPLEMENTATION.
  METHOD run.
    bar = 2.
    WRITE / bar.
    WRITE / intf~bar.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  clas=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n2");
  });

  it("class, alias field from interface, instance", async () => {
    const code = `
INTERFACE intf.
  DATA bar TYPE i.
ENDINTERFACE.

CLASS clas DEFINITION.
  PUBLIC SECTION.
    INTERFACES intf.
    ALIASES bar FOR intf~bar.
    METHODS inst.
    CLASS-METHODS run.
ENDCLASS.

CLASS clas IMPLEMENTATION.
  METHOD inst.
    bar = 2.
    WRITE bar.
  ENDMETHOD.
  METHOD run.
    DATA clas TYPE REF TO clas.
    CREATE OBJECT clas.
    clas->inst( ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  clas=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("constant defined in UPPER case", async () => {
    const code = `
CLASS clas DEFINITION.
  PUBLIC SECTION.
    CONSTANTS UPPER TYPE c LENGTH 1 VALUE 'U'.
ENDCLASS.
CLASS clas IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  WRITE clas=>upper.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("U");
  });

  it("static method in interface", async () => {
    const code = `
INTERFACE lif_test.
  CLASS-METHODS main.
ENDINTERFACE.

CLASS lcl_test DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_test.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.

  METHOD lif_test~main.
    WRITE 'I am from interface'.
  ENDMETHOD.

  METHOD main.
    WRITE 'I am from class'.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_test=>lif_test~main( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("I am from interface");
  });

  it("\"simple\" importing parameter", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS bar IMPORTING foo TYPE simple.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD bar.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_bar=>bar( 2 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("\"numeric\" importing parameter", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS bar IMPORTING foo TYPE numeric.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD bar.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_bar=>bar( 2 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("static method with constant default", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CONSTANTS default_height TYPE i VALUE 3.
    CLASS-METHODS create
      IMPORTING height TYPE i DEFAULT default_height.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD create.
    WRITE height.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_bar=>create( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("static method with structured constant default", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF defaults,
                 height TYPE i VALUE 3,
               END OF defaults.
    CLASS-METHODS create
      IMPORTING height TYPE i DEFAULT defaults-height.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD create.
    WRITE height.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_bar=>create( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("CONSTANTS VALUE IS INITIAL", async () => {
    const code = `
  CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_dummy,
        tdate TYPE d,
      END OF ty_dummy.
    CONSTANTS c_dummy TYPE ty_dummy VALUE IS INITIAL.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("something strange", async () => {
    const zcl_abapgit_ajson = `
CLASS zcl_abapgit_ajson DEFINITION PUBLIC.
ENDCLASS.
CLASS zcl_abapgit_ajson IMPLEMENTATION.
ENDCLASS.`;

    const locals = `INTERFACE lif_kind.
  TYPES ty_kind TYPE c LENGTH 1.
  CONSTANTS: any TYPE ty_kind VALUE 'A'.
ENDINTERFACE.

CLASS lcl_json_serializer DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
  PRIVATE SECTION.
    CLASS-DATA gv_comma_with_lf TYPE string.
ENDCLASS.

CLASS lcl_json_serializer IMPLEMENTATION.
  METHOD class_constructor.
    gv_comma_with_lf = ',' && cl_abap_char_utilities=>newline.
  ENDMETHOD.
ENDCLASS.`;

    const cl_abap_char_utilities = `CLASS cl_abap_char_utilities DEFINITION PUBLIC.
  PUBLIC SECTION.
    CONSTANTS newline TYPE c LENGTH 1 VALUE '_'.
    CLASS-METHODS class_constructor.
ENDCLASS.
CLASS cl_abap_char_utilities IMPLEMENTATION.
  METHOD class_constructor.
    WRITE '@KERNEL cl_abap_char_utilities.newline.set("\\n");'.
  ENDMETHOD.
ENDCLASS.`;

    const result = await compileFiles([
      {filename: "zcl_abapgit_ajson.clas.abap", contents: zcl_abapgit_ajson},
      {filename: "cl_abap_char_utilities.clas.abap", contents: cl_abap_char_utilities},
      {filename: "zcl_abapgit_ajson.clas.locals_imp.abap", contents: locals},
    ]);

    const js = result.objects[1].chunk.getCode();
    expect(js).to.contain("abap.Classes['CL_ABAP_CHAR_UTILITIES'].newline");
  });

  it("local abstract class, no implementation", async () => {
    const code = `
CLASS lcl_zip DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS read ABSTRACT.
ENDCLASS.

CLASS lcl DEFINITION INHERITING FROM lcl_zip.
  PUBLIC SECTION.
    METHODS read REDEFINITION.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD read.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escaped preferred parameter, check valid JS syntax", async () => {
    const code = `
INTERFACE zif_abaplint_code_inspector.
  METHODS run
    IMPORTING
      in1 TYPE string optional
      in2 type string optional
      PREFERRED PARAMETER !in1.
endinterface.

FORM bar.
  DATA li_code_inspector TYPE REF TO zif_abaplint_code_inspector.
  li_code_inspector->run( |sdf| ).
ENDFORM.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test escaping of namespace in class constants", async () => {
    const code = `
CLASS /dsdf/sdf DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF pretty_mode,
                 low_case TYPE string VALUE 'low_case',
               END OF pretty_mode.
ENDCLASS.

CLASS /dsdf/sdf IMPLEMENTATION.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test escaping of namespace in interface constants", async () => {
    const code = `
INTERFACE /dsdf/sdf.
  CONSTANTS: BEGIN OF pretty_mode,
               low_case TYPE string VALUE 'low_case',
             END OF pretty_mode.
ENDINTERFACE.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("C type input, should accept longer than one char", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING s TYPE c.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE s.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( 'hello' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("should call super class constructor with identical input", async () => {
    const code = `
CLASS lcl_top DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING foo TYPE i.
ENDCLASS.

CLASS lcl_top IMPLEMENTATION.
  METHOD constructor.
    ASSERT foo IS NOT INITIAL.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sub DEFINITION INHERITING FROM lcl_top.
ENDCLASS.

CLASS lcl_sub IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl_sub.
  CREATE OBJECT ref EXPORTING foo = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("constructor call, different input parameter names", async () => {
    const code = `
CLASS lcl_sup DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING foo TYPE i.
ENDCLASS.

CLASS lcl_sup IMPLEMENTATION.
  METHOD constructor.
    WRITE foo.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sub DEFINITION INHERITING FROM lcl_sup.
  PUBLIC SECTION.
    METHODS constructor IMPORTING bar TYPE i.
ENDCLASS.

CLASS lcl_sub IMPLEMENTATION.
  METHOD constructor.
    super->constructor( bar ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA sub TYPE REF TO lcl_sub.
  CREATE OBJECT sub EXPORTING bar = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("escape namespace in method name", async () => {
    const intf = `
INTERFACE /dsdf/intf PUBLIC.
  METHODS foo.
ENDINTERFACE.`;
    const clas = `
CLASS /dsdf/clas DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES /dsdf/intf.
ENDCLASS.
CLASS /dsdf/clas IMPLEMENTATION.
  METHOD /dsdf/intf~foo.
    /dsdf/intf~foo( ).
  ENDMETHOD.
ENDCLASS.`;
    const result = await compileFiles([
      {filename: "#dsdf#intf.intf.abap", contents: intf},
      {filename: "#dsdf#clas.clas.abap", contents: clas},
    ]);

    const js = result.objects[1].chunk.getCode();
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escape namespace in intf attribute", async () => {
    const intf = `
INTERFACE /dsdf/intf PUBLIC.
  DATA bar TYPE i.
ENDINTERFACE.`;
    const clas = `
CLASS /dsdf/clas DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES /dsdf/intf.
    METHODS foo.
ENDCLASS.
CLASS /dsdf/clas IMPLEMENTATION.
  METHOD foo.
    /dsdf/intf~bar = 2.
  ENDMETHOD.
ENDCLASS.`;
    const result = await compileFiles([
      {filename: "#dsdf#intf.intf.abap", contents: intf},
      {filename: "#dsdf#clas.clas.abap", contents: clas},
    ]);

    const js = result.objects[1].chunk.getCode();
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escape namespace in source", async () => {
    const intf = `
INTERFACE /dsdf/intf PUBLIC.
  CONSTANTS bar TYPE string VALUE 'hi'.
ENDINTERFACE.`;
    const clas = `
CLASS /dsdf/clas DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES /dsdf/intf.
    METHODS foo.
ENDCLASS.
CLASS /dsdf/clas IMPLEMENTATION.
  METHOD foo.
    DATA lv_foo TYPE string.
    CONCATENATE /dsdf/intf=>bar lv_foo INTO lv_foo.
  ENDMETHOD.
ENDCLASS.`;
    const result = await compileFiles([
      {filename: "#dsdf#intf.intf.abap", contents: intf},
      {filename: "#dsdf#clas.clas.abap", contents: clas},
    ]);

    const js = result.objects[1].chunk.getCode();
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escape namespace in me->, source position", async () => {
    const intf = `
INTERFACE /dsdf/intf PUBLIC.
  DATA bar TYPE string.
ENDINTERFACE.`;
    const clas = `
CLASS /dsdf/clas DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES /dsdf/intf.
    METHODS foo.
ENDCLASS.
CLASS /dsdf/clas IMPLEMENTATION.
  METHOD foo.
    WRITE me->/dsdf/intf~bar.
  ENDMETHOD.
ENDCLASS.`;
    const result = await compileFiles([
      {filename: "#dsdf#intf.intf.abap", contents: intf},
      {filename: "#dsdf#clas.clas.abap", contents: clas},
    ]);

    const js = result.objects[1].chunk.getCode();
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escape namespace in me->, target position", async () => {
    const intf = `
INTERFACE /dsdf/intf PUBLIC.
  DATA bar TYPE string.
ENDINTERFACE.`;
    const clas = `
CLASS /dsdf/clas DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES /dsdf/intf.
    METHODS foo.
ENDCLASS.
CLASS /dsdf/clas IMPLEMENTATION.
  METHOD foo.
    me->/dsdf/intf~bar = 'hello'.
  ENDMETHOD.
ENDCLASS.`;
    const result = await compileFiles([
      {filename: "#dsdf#intf.intf.abap", contents: intf},
      {filename: "#dsdf#clas.clas.abap", contents: clas},
    ]);

    const js = result.objects[1].chunk.getCode();
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escape aliased method", async () => {
    const intf = `
INTERFACE /dsdf/intf PUBLIC.
  METHODS method1.
ENDINTERFACE.`;
    const clas = `
CLASS /dsdf/clas DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES /dsdf/intf.
    ALIASES method1 FOR /dsdf/intf~method1.
ENDCLASS.
CLASS /dsdf/clas IMPLEMENTATION.
  METHOD /dsdf/intf~method1.
  ENDMETHOD.
ENDCLASS.`;
    const result = await compileFiles([
      {filename: "#dsdf#intf.intf.abap", contents: intf},
      {filename: "#dsdf#clas.clas.abap", contents: clas},
    ]);

    const js = result.objects[1].chunk.getCode();
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("local class, default from class constant", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_actvt,
        create TYPE i VALUE '01',
      END OF gc_actvt.

    CLASS-METHODS check
      IMPORTING
        iv_actvt TYPE i DEFAULT GC_ACTVT-create.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD check.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>check( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("call method, find default parameter name", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo IMPORTING foo TYPE i.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE foo.
  ENDMETHOD.

  METHOD run.
    CALL METHOD foo( 5 ) .
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("data with value, static", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA mc_hello TYPE string READ-ONLY VALUE 'hello'.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  WRITE lcl=>mc_hello.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("data with value, instance", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA mc_hello TYPE string READ-ONLY VALUE 'hello'.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA foo TYPE REF TO lcl.
  CREATE OBJECT foo.
  WRITE foo->mc_hello.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it.skip("top level constants in locals imp", async () => {
    const clas = `
CLASS zcl_alint_lexer DEFINITION PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_alint_lexer IMPLEMENTATION.
ENDCLASS.`;

    const locals = `
CONSTANTS BEGIN OF mode.
  CONSTANTS normal TYPE i VALUE 1.
  CONSTANTS ping TYPE i VALUE 2.
  CONSTANTS str TYPE i VALUE 3.
CONSTANTS END OF mode.`;

    const result = await compileFiles([
      {filename: "zcl_alint_lexer.clas.abap", contents: clas},
      {filename: "zcl_alint_lexer.clas.locals_imp.abap", contents: locals},
    ]);

    const js = result.objects[1].chunk.getCode();
    expect(js).to.contain("mode");
  });

  it("aliased redefinition", async () => {
    const code = `
INTERFACE ifile.
  METHODS getraw RETURNING VALUE(foo) TYPE string.
ENDINTERFACE.

CLASS abstractfile DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES ifile.
    ALIASES getraw FOR ifile~getraw.
ENDCLASS.
CLASS abstractfile IMPLEMENTATION.
  METHOD ifile~getraw.
  ENDMETHOD.
ENDCLASS.

CLASS memoryfile DEFINITION INHERITING FROM abstractfile.
  PUBLIC SECTION.
    METHODS getraw REDEFINITION.
ENDCLASS.
CLASS memoryfile IMPLEMENTATION.
  METHOD getraw.
    foo = |hello|.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA mf TYPE REF TO memoryfile.
  CREATE OBJECT mf.
  WRITE mf->getraw( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("namespace prefixed constant", async () => {
    const code = `
CLASS foo DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF /foo/bar,
                 msgid TYPE i VALUE 2,
               END OF /foo/bar.
    METHODS m.
ENDCLASS.

CLASS foo IMPLEMENTATION.
  METHOD m.
    DATA foo LIKE /foo/bar.
    foo = /foo/bar.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    // check no syntax errors
    await f(abap);
  });

  it("namespaced interface target variable, static", async () => {
    const code = `
INTERFACE /foo/bar.
  CLASS-DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
ENDINTERFACE.

CLASS cls DEFINITION.
  PUBLIC SECTION.
    INTERFACES /foo/bar.
    METHODS m.
ENDCLASS.

CLASS cls IMPLEMENTATION.
  METHOD m.
    DATA wa TYPE i.
    APPEND wa TO /foo/bar~tab.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    // check no syntax errors
    await f(abap);
  });

  it("early CHECK in constructor", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS out.
  PRIVATE SECTION.
    DATA val TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    val = 2.
    CHECK val <> 2.
  ENDMETHOD.
  METHOD out.
    WRITE val.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA foo TYPE REF TO lcl.
  CREATE OBJECT foo.
  foo->out( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("doubly implemented sub sub interface classes", async () => {
    const code = `
INTERFACE lif.
  DATA field TYPE i.
ENDINTERFACE.

INTERFACE subif.
  INTERFACES lif.
ENDINTERFACE.

CLASS top DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    METHODS constructor IMPORTING foo TYPE i.
ENDCLASS.
CLASS top IMPLEMENTATION.
  METHOD constructor.
    lif~field = foo.
  ENDMETHOD.
ENDCLASS.

CLASS sub DEFINITION INHERITING FROM top.
  PUBLIC SECTION.
    INTERFACES subif.
ENDCLASS.
CLASS sub IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO sub.
  CREATE OBJECT lo EXPORTING foo = 2.
  WRITE lo->lif~field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("structured default values", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      BEGIN OF gs_test_data,
        text       TYPE string VALUE 'foo',
        empty_text TYPE string VALUE '',
      END OF gs_test_data.
    CLASS-DATA bar TYPE string VALUE 'bar'.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  WRITE lcl=>gs_test_data-text.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("importing VALUE with redefinition", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo IMPORTING VALUE(bar) TYPE abap_bool.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD foo.
  ENDMETHOD.
ENDCLASS.

CLASS sub DEFINITION INHERITING FROM lcl.
  PUBLIC SECTION.
    METHODS foo REDEFINITION.
ENDCLASS.
CLASS sub IMPLEMENTATION.
  METHOD foo.
    bar = abap_true.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO sub.
  CREATE OBJECT lo.
  ASSERT abap_false = ''.
  lo->foo( abap_false ).
  ASSERT abap_false = ''.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("importing VALUE with set", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING VALUE(foo) TYPE abap_bool DEFAULT abap_false.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    foo = abap_true.
    ASSERT abap_true = 'X'.
    ASSERT abap_false = ''.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("set static via instance", async () => {
    const code = `
CLASS lcl_static DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA foo TYPE string.
ENDCLASS.
CLASS lcl_static IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl_static.
  CREATE OBJECT ref.
  ref->foo = 'hello'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Class, simple method call", async () => {
    const code = `
    INTERFACE /tst/test PUBLIC.
      CONSTANTS test_constant TYPE string VALUE 'working'.
    ENDINTERFACE.


    CLASS /tst/cl_test DEFINITION.
      PUBLIC SECTION.
        INTERFACES /tst/test.

        ALIASES test_constant
          FOR /tst/test~test_constant .
        METHODS
                run.
    ENDCLASS.

    CLASS /tst/cl_test IMPLEMENTATION.
      METHOD run.
        WRITE test_constant.
      ENDMETHOD.
    ENDCLASS.

    DATA foo TYPE REF TO /tst/cl_test.
    CREATE OBJECT foo.
    foo->run( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("working");
  });

  it("Class, static attribute accessed via me", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING foobar TYPE i.
    CLASS-DATA foobar TYPE i.
    DATA inst TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    me->foobar = foobar.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl.
  CREATE OBJECT obj EXPORTING foobar = 2.
  WRITE obj->foobar.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("Class, x generic", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING foobar TYPE x.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    WRITE foobar.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( 'AABB' ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("AABB");
  });

  it("Class, exporting", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo EXPORTING bar TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    bar = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Class, generic importing", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING bar TYPE csequence OPTIONAL.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    ASSERT bar IS INITIAL.
    ASSERT bar = ''.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Class, ref to obj", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo IMPORTING bar TYPE REF TO object.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->foo( lo ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Class, optional", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING bar TYPE string OPTIONAL.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    ASSERT bar IS INITIAL.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Class, convertion char to xstring when calling method", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo IMPORTING xstr TYPE xstring.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    DATA int TYPE i.
    int = xstr(2).
    ASSERT int = 4676.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->foo( '1244' ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Class, default abap_undefined", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS use_lxe
      IMPORTING iv_yes TYPE abap_bool DEFAULT abap_undefined.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD use_lxe.
    ASSERT iv_yes = abap_undefined.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->use_lxe( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Class, changing with default integer", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS moo CHANGING def TYPE i DEFAULT 1.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD moo.
    def = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>moo( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Class, same field input to importing and exporting", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty,
             field TYPE i,
           END OF ty.
    CLASS-METHODS foo IMPORTING bar TYPE ty
                      EXPORTING moo TYPE ty.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE / bar-field.
    WRITE / moo-field.
    CLEAR moo.
    WRITE / bar-field.
    WRITE / moo-field.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA dat TYPE lcl=>ty.
  dat-field = 2.
  lcl=>foo(
    EXPORTING bar = dat
    IMPORTING moo = dat ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("2\n2\n0\n0");
  });

  it("Class, tables in+out", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES ty TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    CLASS-METHODS foo IMPORTING bar TYPE ty
                      EXPORTING moo TYPE ty.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE / lines( bar ).
    WRITE / lines( moo ).
    moo = bar.
    WRITE / lines( bar ).
    WRITE / lines( moo ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA dat TYPE lcl=>ty.
  APPEND 'sdfsdfsdf' TO dat.
  lcl=>foo(
    EXPORTING bar = dat
    IMPORTING moo = dat ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("1\n1\n1\n1");
  });

  it("Class, numeric generic, passed char", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS moo IMPORTING foo TYPE numeric.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD moo.
    DATA lv_type TYPE c LENGTH 1.
    DATA lv_decimals TYPE i.
    DESCRIBE FIELD foo TYPE lv_type.
    WRITE / lv_type.
    DESCRIBE FIELD foo DECIMALS lv_decimals.
    WRITE / lv_decimals.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>moo( '11.11' ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("P\n2");
  });

  it("Class, tables in+out, clear", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES ty TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    CLASS-METHODS foo IMPORTING bar TYPE ty
                      EXPORTING moo TYPE ty.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE / lines( bar ).
    WRITE / lines( moo ).
    CLEAR moo.
    WRITE / lines( bar ).
    WRITE / lines( moo ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA dat TYPE lcl=>ty.
  APPEND 'sdfsdfsdf' TO dat.
  lcl=>foo(
    EXPORTING bar = dat
    IMPORTING moo = dat ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("1\n1\n0\n0");
  });

  it.skip("Class, inheritence and aliases redefintion", async () => {
    const code = `
INTERFACE lif.
  METHODS get_text.
ENDINTERFACE.

CLASS top DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    ALIASES get_text FOR lif~get_text.
ENDCLASS.
CLASS top IMPLEMENTATION.
  METHOD lif~get_text.
    WRITE 'top'.
  ENDMETHOD.
ENDCLASS.

CLASS middle DEFINITION INHERITING FROM top.
ENDCLASS.
CLASS middle IMPLEMENTATION.
ENDCLASS.

CLASS sub DEFINITION INHERITING FROM middle.
  PUBLIC SECTION.
    METHODS get_text REDEFINITION.
ENDCLASS.
CLASS sub IMPLEMENTATION.
  METHOD get_text.
    WRITE 'sub'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO sub.
  CREATE OBJECT lo.
  lo->get_text( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("sub");
  });

  it.only("Class, private method in inheritance", async () => {
    const code = `
CLASS top DEFINITION.
  PUBLIC SECTION.
    METHODS call_a_private_method RETURNING VALUE(result) TYPE string.
  PRIVATE SECTION.
    METHODS my_private_method RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS top IMPLEMENTATION.
  METHOD call_a_private_method.
    result = my_private_method( ).
  ENDMETHOD.

  METHOD my_private_method.
    result = 'TOP'.
  ENDMETHOD.
ENDCLASS.

CLASS sub DEFINITION INHERITING FROM top.
  PUBLIC SECTION.
  PRIVATE SECTION.
    METHODS my_private_method RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS sub IMPLEMENTATION.
  METHOD my_private_method.
    result = 'SUB'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO sub.
  DATA result TYPE string.
  CREATE OBJECT lo.
  result = lo->call_a_private_method( ).
  WRITE result.`;

    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("TOP");
  });

  it("Class, private method in inheritance redefined", async () => {
    const code = `

CLASS top DEFINITION.
  PUBLIC SECTION.
    METHODS call_a_private_method RETURNING VALUE(result) TYPE string.
  PRIVATE SECTION.
    METHODS my_private_method RETURNING VALUE(result) TYPE string.

ENDCLASS.
CLASS top IMPLEMENTATION.
  METHOD call_a_private_method.
    result = my_private_method( ).
  ENDMETHOD.

  METHOD my_private_method.
    result = 'TOP'.
  ENDMETHOD.
ENDCLASS.

CLASS sub DEFINITION INHERITING FROM top.
  PUBLIC SECTION.
    METHODS call_a_private_method REDEFINITION.
  PRIVATE SECTION.
    METHODS my_private_method RETURNING VALUE(result) TYPE string.

ENDCLASS.
CLASS sub IMPLEMENTATION.
  METHOD call_a_private_method.
    result = my_private_method( ).
  ENDMETHOD.

  METHOD my_private_method.
    result = 'SUB'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO sub.
  DATA result TYPE string.
  CREATE OBJECT lo.
  result = lo->call_a_private_method( ).
  WRITE result.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("SUB");
  });

});
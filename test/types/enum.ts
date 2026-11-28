import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, compileFiles, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - ENUMs", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basics", async () => {
    const code = `
TYPES: BEGIN OF ENUM ty_cache_policy STRUCTURE cache_policies,
         use_all,
         use_none,
       END OF ENUM ty_cache_policy STRUCTURE cache_policies.

DATA foo TYPE ty_cache_policy.
WRITE / cache_policies-use_all.
WRITE / foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("USE_ALL\nUSE_ALL");
  });

  it("basics, in a class", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM ty_foo STRUCTURE enumvalues,
             value1,
             value2,
           END OF ENUM ty_foo STRUCTURE enumvalues.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  ASSERT lcl=>enumvalues-value1 = lcl=>enumvalues-value1.
  ASSERT lcl=>enumvalues-value1 <> lcl=>enumvalues-value2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("values of an enum in a local interface are distinct", async () => {
    const code = `
INTERFACE lif_role.
  TYPES:
    BEGIN OF ENUM ty_role,
      dummy,
      stub,
      spy,
    END OF ENUM ty_role.
ENDINTERFACE.

START-OF-SELECTION.
  DATA role TYPE lif_role=>ty_role.
  IF role = lif_role=>dummy.
    WRITE / 'initial is the first value'.
  ENDIF.
  role = lif_role=>spy.
  IF role <> lif_role=>stub.
    WRITE / 'spy is not stub'.
  ENDIF.
  IF role = lif_role=>spy.
    WRITE / 'spy is spy'.
  ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("initial is the first value\nspy is not stub\nspy is spy");
  });

  it("values of an enum in a local class are distinct", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ENUM ty_size,
        small,
        large,
      END OF ENUM ty_size.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA size TYPE lcl=>ty_size.
  IF size = lcl=>small.
    WRITE / 'initial is the first value'.
  ENDIF.
  size = lcl=>large.
  IF size <> lcl=>small.
    WRITE / 'large is not small'.
  ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("initial is the first value\nlarge is not small");
  });

  it("two enums in one interface, each numbered from its first value", async () => {
    const code = `
INTERFACE lif_enums.
  TYPES:
    BEGIN OF ENUM ty_color,
      red,
      green,
    END OF ENUM ty_color.
  TYPES:
    BEGIN OF ENUM ty_size,
      small,
      large,
    END OF ENUM ty_size.
ENDINTERFACE.

START-OF-SELECTION.
  DATA size TYPE lif_enums=>ty_size.
  IF size = lif_enums=>small.
    WRITE / 'initial is the first value'.
  ENDIF.
  size = lif_enums=>large.
  IF size <> lif_enums=>small.
    WRITE / 'large is not small'.
  ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("initial is the first value\nlarge is not small");
  });

  it("enum of a global interface, values copied into a class implementing it", async () => {
    const intf = `
INTERFACE zif_enum_roles PUBLIC.
  TYPES:
    BEGIN OF ENUM ty_role,
      first,
      second,
    END OF ENUM ty_role.
ENDINTERFACE.`;
    const clas = `
CLASS zcl_enum_impl DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_enum_roles.
ENDCLASS.
CLASS zcl_enum_impl IMPLEMENTATION.
ENDCLASS.`;
    const result = await compileFiles([
      {filename: "zif_enum_roles.intf.abap", contents: intf},
      {filename: "zcl_enum_impl.clas.abap", contents: clas},
    ]);
    const js = result.objects.find(o => o.object.name === "ZCL_ENUM_IMPL")?.chunk.getCode();
    expect(js).to.contain("zcl_enum_impl.zif_enum_roles$first.set(0);");
    expect(js).to.contain("zcl_enum_impl.zif_enum_roles$second.set(1);");
  });

  it("string template writes an enum value of a class as the name of its constant", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ENUM ty_size,
        small,
        large,
      END OF ENUM ty_size.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA size TYPE lcl=>ty_size.
  WRITE / |{ size }|.
  size = lcl=>large.
  WRITE / |{ size }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("SMALL\nLARGE");
  });

  it("string template, two enums in one interface, each with its own names", async () => {
    const code = `
INTERFACE lif_enums.
  TYPES:
    BEGIN OF ENUM ty_color,
      red,
      green,
    END OF ENUM ty_color.
  TYPES:
    BEGIN OF ENUM ty_size,
      small,
      large,
    END OF ENUM ty_size.
ENDINTERFACE.

START-OF-SELECTION.
  DATA color TYPE lif_enums=>ty_color.
  DATA size TYPE lif_enums=>ty_size.
  color = lif_enums=>green.
  size = lif_enums=>large.
  WRITE / |{ color } { size }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("GREEN LARGE");
  });

  it("string template, enum with STRUCTURE as a component, read through a generic field symbol", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ENUM ty_light STRUCTURE light,
        red,
        green,
      END OF ENUM ty_light STRUCTURE light.
    TYPES:
      BEGIN OF ty_signal,
        light TYPE ty_light,
      END OF ty_signal.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA signal TYPE lcl=>ty_signal.
  DATA copy TYPE lcl=>ty_signal.
  FIELD-SYMBOLS <light> TYPE simple.
  ASSIGN COMPONENT 'LIGHT' OF STRUCTURE signal TO <light>.
  WRITE / |{ <light> }|.
  signal-light = lcl=>light-green.
  copy = signal.
  ASSIGN COMPONENT 'LIGHT' OF STRUCTURE copy TO <light>.
  WRITE / |{ <light> }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("RED\nGREEN");
  });

  it("enum of a global interface, the type used in a class names the constants", async () => {
    const intf = `
INTERFACE zif_enum_roles PUBLIC.
  TYPES:
    BEGIN OF ENUM ty_role,
      first,
      second,
    END OF ENUM ty_role.
ENDINTERFACE.`;
    const clas = `
CLASS zcl_enum_user DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS zcl_enum_user IMPLEMENTATION.
  METHOD run.
    DATA role TYPE zif_enum_roles=>ty_role.
    WRITE / |{ role }|.
  ENDMETHOD.
ENDCLASS.`;
    const result = await compileFiles([
      {filename: "zif_enum_roles.intf.abap", contents: intf},
      {filename: "zcl_enum_user.clas.abap", contents: clas},
    ]);
    const js = result.objects.find(o => o.object.name === "ZCL_ENUM_USER")?.chunk.getCode();
    expect(js).to.contain(`new abap.types.String({qualifiedName: "ZIF_ENUM_ROLES=>TY_ROLE", enumValues: ["FIRST","SECOND"]})`);
  });

  it("enum with STRUCTURE of an interface, initial value of a component equals the first constant", async () => {
    const code = `
INTERFACE lif_states.
  TYPES:
    BEGIN OF ENUM ty_state STRUCTURE states,
      pending,
      done,
    END OF ENUM ty_state STRUCTURE states.
  TYPES:
    BEGIN OF ty_row,
      name  TYPE string,
      state TYPE ty_state,
    END OF ty_row.
  TYPES ty_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS count
      IMPORTING rows          TYPE lif_states=>ty_rows
                wanted        TYPE lif_states=>ty_state
      RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD count.
    result = REDUCE #( INIT n = 0
                       FOR r IN rows WHERE ( state = wanted )
                       NEXT n = n + 1 ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA rows TYPE lif_states=>ty_rows.
  DATA row TYPE lif_states=>ty_row.
  INSERT VALUE #( name = \`a\` ) INTO TABLE rows.
  row-name = \`b\`.
  INSERT row INTO TABLE rows.
  INSERT VALUE #( name = \`c\` state = lif_states=>states-done ) INTO TABLE rows.
  IF rows[ 1 ]-state = lif_states=>states-pending.
    WRITE / 'initial is pending'.
  ENDIF.
  WRITE / lcl=>count( rows = rows wanted = lif_states=>states-pending ).
  WRITE / lcl=>count( rows = rows wanted = lif_states=>states-done ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("initial is pending\n2\n1");
  });

});

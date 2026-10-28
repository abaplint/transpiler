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

});

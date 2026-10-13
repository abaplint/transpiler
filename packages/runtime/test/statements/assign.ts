import {expect} from "chai";
import {ABAP} from "../../src";

describe("Statement ASSIGN COMPONENT", () => {
  let abap: ABAP;

  beforeEach(() => {
    abap = new ABAP();
    (globalThis as any).abap = abap;
  });

  // A structure built at runtime (cl_abap_structdescr=>create) may name a
  // component with a dash, as RAP's %element-<field> rows do; a static DATA
  // ... BEGIN OF cannot, so the structure is built here directly
  it("finds a component whose name has a dash", () => {
    const value = new abap.types.Character(1).set("X");
    const source = new abap.types.Structure({"bar": new abap.types.Character(1), "%element-customerid": value});
    const target = new abap.types.FieldSymbol(new abap.types.Character(1));
    abap.statements.assign({component: "%ELEMENT-CUSTOMERID", source, target});
    expect(abap.builtin.sy.get().subrc.get()).to.equal(0);
    expect(target.getPointer()).to.equal(value);
  });

  it("walks a dash as a path when no component has the whole name", () => {
    const baz = new abap.types.String().set("deep");
    const source = new abap.types.Structure({"bar": new abap.types.Structure({"baz": baz})});
    const target = new abap.types.FieldSymbol(new abap.types.String());
    abap.statements.assign({component: "BAR-BAZ", source, target});
    expect(abap.builtin.sy.get().subrc.get()).to.equal(0);
    expect(target.getPointer()).to.equal(baz);
  });

  it("a missing step on the path is sy-subrc 4", () => {
    const source = new abap.types.Structure({"bar": new abap.types.Structure({"baz": new abap.types.String()})});
    const target = new abap.types.FieldSymbol(new abap.types.String());
    abap.statements.assign({component: "NOPE-BAZ", source, target});
    expect(abap.builtin.sy.get().subrc.get()).to.equal(4);
    expect(target.getPointer()).to.equal(undefined);
  });
});

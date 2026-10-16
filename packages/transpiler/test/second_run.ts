import {expect} from "chai";
import {Transpiler} from "../src";
import * as abaplint from "@abaplint/core";

const intf = (dflt: number) => new abaplint.MemoryFile("zif_shape.intf.abap", `
INTERFACE zif_shape PUBLIC.
  METHODS m IMPORTING a TYPE i DEFAULT ${dflt}.
ENDINTERFACE.`);

const impl = new abaplint.MemoryFile("zcl_impl.clas.abap", `
CLASS zcl_impl DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_shape.
ENDCLASS.
CLASS zcl_impl IMPLEMENTATION.
  METHOD zif_shape~m.
  ENDMETHOD.
ENDCLASS.`);

const alone = new abaplint.MemoryFile("zcl_alone.clas.abap", `
CLASS zcl_alone DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS zcl_alone IMPLEMENTATION.
  METHOD run.
    DATA x TYPE i.
  ENDMETHOD.
ENDCLASS.`);

const registry = () => new abaplint.Registry().addFiles([intf(1), impl, alone]);
// the syntax result abaplint keeps on an object: a new one means it was checked again
const syntaxOf = (reg: abaplint.IRegistry, name: string) => (reg.getObject("CLAS", name) as abaplint.ABAPObject).syntaxResult;
const code = (res: {objects: readonly {filename: string, chunk: {getCode(): string}}[]}, name: string) =>
  res.objects.find((o) => o.filename.startsWith(name))!.chunk.getCode();
const shapeAndReaders = (obj: abaplint.IObject) => ["ZIF_SHAPE", "ZCL_IMPL"].includes(obj.getName());

describe("a second run over the same registry", () => {

  it("without only, checks every object again", async () => {
    const reg = registry();
    await new Transpiler().run(reg);
    const before = syntaxOf(reg, "ZCL_ALONE");
    expect(before).to.not.equal(undefined);
    await new Transpiler().run(reg);
    expect(syntaxOf(reg, "ZCL_ALONE")).to.not.equal(before);
  });

  it("with only, checks the chosen objects again and keeps the rest", async () => {
    const reg = registry();
    await new Transpiler().run(reg);
    const alone = syntaxOf(reg, "ZCL_ALONE");
    const impl = syntaxOf(reg, "ZCL_IMPL");
    await new Transpiler({only: shapeAndReaders}).run(reg);
    expect(syntaxOf(reg, "ZCL_ALONE")).to.equal(alone);
    expect(syntaxOf(reg, "ZCL_IMPL")).to.not.equal(impl);
  });

  it("with only, gives a reader of a changed interface the output a new registry gives", async () => {
    const reg = registry();
    await new Transpiler().run(reg);
    reg.updateFile(intf(2));
    const second = await new Transpiler({only: shapeAndReaders}).run(reg);
    const fresh = await new Transpiler().run(new abaplint.Registry().addFiles([intf(2), impl, alone]));
    expect(code(second, "zcl_impl")).to.include("IntegerFactory.get(2)");
    expect(code(second, "zcl_impl")).to.equal(code(fresh, "zcl_impl"));
    expect(second.initializationScript).to.equal(fresh.initializationScript);
  });

});

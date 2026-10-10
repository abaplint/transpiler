import {expect} from "chai";
import {Transpiler} from "../src";
import * as abaplint from "@abaplint/core";
import {IOutput} from "../src/types";

const clas = (name: string, body: string) => new abaplint.MemoryFile(name.toLowerCase() + ".clas.abap", `
CLASS ${name} DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS ${name} IMPLEMENTATION.
  METHOD run.
    ${body}
  ENDMETHOD.
ENDCLASS.`);

async function run(only?: (obj: abaplint.IObject) => boolean): Promise<IOutput> {
  const reg = new abaplint.Registry().addFiles([
    clas("ZCL_A", "DO 2 TIMES. ENDDO."),
    clas("ZCL_B", "zcl_a=>run( ). DO 3 TIMES. ENDDO."),
    clas("ZCL_C", "DATA x TYPE i. x = 1."),
  ]);
  return new Transpiler({only}).run(reg);
}

describe("only", () => {

  it("builds the output of the objects it names, the same as a full run does", async () => {
    const full = await run();
    const part = await run(obj => obj.getName() === "ZCL_B");
    expect(part.objects.map(o => o.object.name)).to.deep.equal(["ZCL_B"]);
    const b = full.objects.find(o => o.object.name === "ZCL_B")!;
    expect(part.objects[0].chunk.getCode()).to.equal(b.chunk.getCode());
  });

  it("still covers the whole registry in the initialization scripts", async () => {
    const full = await run();
    const part = await run(obj => obj.getName() === "ZCL_B");
    expect(part.initializationScript).to.equal(full.initializationScript);
    expect(part.initializationScript2).to.equal(full.initializationScript2);
  });

});

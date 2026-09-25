import {expect} from "chai";
import {Transpiler} from "../src";
import * as abaplint from "@abaplint/core";
import {IFile} from "../src/types";

async function codeOf(files: IFile[], name: string) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory);
  const res = await new Transpiler().run(reg);
  return res.objects.find(o => o.object.name === name)!.chunk.getCode();
}

// a DO loop takes a temporary name for its counter
const clas = (name: string) => ({filename: name.toLowerCase() + ".clas.abap", contents: `
CLASS ${name} DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS ${name} IMPLEMENTATION.
  METHOD run.
    DO 2 TIMES.
    ENDDO.
  ENDMETHOD.
ENDCLASS.`});

describe("temporary names", () => {

  it("an object's output does not depend on the objects built before it", async () => {
    const alone = await codeOf([clas("ZCL_B")], "ZCL_B");
    const after = await codeOf([clas("ZCL_A"), clas("ZCL_B")], "ZCL_B");
    expect(after).to.equal(alone);
  });

  it("a program's top-level loop does not depend on the objects built before it", async () => {
    const prog = {filename: "zprog.prog.abap", contents: `REPORT zprog.
DO 2 TIMES.
ENDDO.`};
    const alone = await codeOf([prog], "ZPROG");
    const after = await codeOf([clas("ZCL_A"), prog], "ZPROG");
    expect(after).to.equal(alone);
  });

  it("a second run in the same process gives the same output", async () => {
    const first = await codeOf([clas("ZCL_A")], "ZCL_A");
    const second = await codeOf([clas("ZCL_A")], "ZCL_A");
    expect(second).to.equal(first);
  });

});

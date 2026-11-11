import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {Transpiler} from "../src";

async function transpileIgnoringSyntax(filename: string, contents: string) {
  const reg = new abaplint.Registry().addFile(new abaplint.MemoryFile(filename, contents));
  return new Transpiler({ignoreSyntaxCheck: true}).run(reg);
}

describe("Definition that cannot be read", () => {

  it("a class whose super class is unknown is named with its file and the missing class", async () => {
    const abap = `CLASS lcl_grid DEFINITION INHERITING FROM zcl_not_here.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl_grid IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.`;
    let message = "";
    try {
      await transpileIgnoringSyntax("zfoobar.prog.abap", abap);
    } catch (e) {
      message = (e as Error).message;
    }
    expect(message).to.include("zfoobar.prog.abap");
    expect(message.toUpperCase()).to.include("LCL_GRID");
    expect(message.toUpperCase()).to.include("ZCL_NOT_HERE");
    expect(message).to.not.include("unexpected undefined");
  });

});

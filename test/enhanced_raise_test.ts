import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}], {skipVersionCheck});
}

const cx_root = `CLASS cx_root DEFINITION ABSTRACT PUBLIC.
  PUBLIC SECTION.
ENDCLASS.

CLASS cx_root IMPLEMENTATION.
ENDCLASS.`;

const cx_static_check = `CLASS cx_static_check DEFINITION PUBLIC INHERITING FROM cx_root ABSTRACT.
  PUBLIC SECTION.
ENDCLASS.

CLASS cx_static_check IMPLEMENTATION.
ENDCLASS.`;

describe("RAISE EXCEPTION MESSAGE - Enhanced Tests", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("RAISE EXCEPTION MESSAGE with type and number", async () => {
    const code = `
${cx_root}
${cx_static_check}

CLASS lcx DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS lcx IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  RAISE EXCEPTION TYPE lcx MESSAGE e123(zzz).`;
    
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail("Exception should have been thrown");
    } catch(e) {
      expect(e.constructor.name).to.contain("lcx");
      // Check that MESSAGE_INFO is set on the exception
      expect(e.MESSAGE_INFO).to.not.be.undefined;
      expect(e.MESSAGE_INFO.type).to.equal("E");
      expect(e.MESSAGE_INFO.number).to.equal("123");
      expect(e.MESSAGE_INFO.id).to.equal("zzz");
    }
  });

  it("RAISE EXCEPTION MESSAGE with WITH parameters", async () => {
    const code = `
${cx_root}
${cx_static_check}

CLASS lcx DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS lcx IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  RAISE EXCEPTION TYPE lcx MESSAGE e123(zzz) WITH 'value1' 'value2'.`;
    
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail("Exception should have been thrown");
    } catch(e) {
      expect(e.constructor.name).to.contain("lcx");
      // Check that MESSAGE_INFO is set with WITH parameters
      expect(e.MESSAGE_INFO).to.not.be.undefined;
      expect(e.MESSAGE_INFO.type).to.equal("E");
      expect(e.MESSAGE_INFO.number).to.equal("123");
      expect(e.MESSAGE_INFO.id).to.equal("zzz");
      expect(e.MESSAGE_INFO.with).to.be.an('array');
      expect(e.MESSAGE_INFO.with).to.have.length(2);
    }
  });

  it("RAISE EXCEPTION MESSAGE ID/TYPE/NUMBER style", async () => {
    const code = `
${cx_root}
${cx_static_check}

CLASS lcx DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS lcx IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  RAISE EXCEPTION TYPE lcx MESSAGE ID 'ZCL' TYPE 'E' NUMBER '001'.`;
    
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail("Exception should have been thrown");
    } catch(e) {
      expect(e.constructor.name).to.contain("lcx");
      // Check that MESSAGE_INFO is set correctly for explicit ID/TYPE/NUMBER
      expect(e.MESSAGE_INFO).to.not.be.undefined;
      expect(e.MESSAGE_INFO.type).to.contain("E");
      expect(e.MESSAGE_INFO.number).to.contain("001");
      expect(e.MESSAGE_INFO.id).to.contain("ZCL");
    }
  });

});
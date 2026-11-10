import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_commit.prog.abap", contents}]);
}

describe("Running statements - COMMIT and ROLLBACK WORK", () => {

  beforeEach(() => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("COMMIT WORK, without any database connection", async () => {
    const js = await run("COMMIT WORK. WRITE 'after'.");
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("after");
  });

  it("ROLLBACK WORK, without any database connection", async () => {
    const js = await run("ROLLBACK WORK. WRITE 'after'.");
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("after");
  });

  it("COMMIT WORK and ROLLBACK WORK set sy-subrc to 0", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      READ TABLE tab INDEX 1 TRANSPORTING NO FIELDS.
      COMMIT WORK.
      WRITE sy-subrc.
      READ TABLE tab INDEX 1 TRANSPORTING NO FIELDS.
      ROLLBACK WORK.
      WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00");
  });

  it("COMMIT WORK AND WAIT, sets sy-subrc", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      READ TABLE tab INDEX 1 TRANSPORTING NO FIELDS.
      COMMIT WORK AND WAIT.
      WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("COMMIT CONNECTION, resolves a dynamic connection name", async () => {
    let commits = 0;
    abap.context.databaseConnections["MYCON"] = {
      commit: async () => { commits++; },
    } as any;

    const code = `
      DATA connection TYPE string VALUE 'mycon'.
      COMMIT CONNECTION (connection).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(commits).to.equal(1);
  });

  it("ROLLBACK CONNECTION, resolves a quoted connection name", async () => {
    let rollbacks = 0;
    abap.context.databaseConnections["MYCON"] = {
      rollback: async () => { rollbacks++; },
    } as any;

    const js = await run("ROLLBACK CONNECTION 'mycon'.");
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(rollbacks).to.equal(1);
  });

  it("PERFORM ON COMMIT runs at COMMIT WORK, by LEVEL, once", async () => {
    const code = `
FORM one.
  WRITE / 'one'.
ENDFORM.
FORM two.
  WRITE / 'two'.
ENDFORM.
FORM late.
  WRITE / 'late'.
ENDFORM.
FORM back.
  WRITE / 'back'.
ENDFORM.

START-OF-SELECTION.
  PERFORM late ON COMMIT LEVEL 2.
  PERFORM one ON COMMIT.
  PERFORM two ON COMMIT.
  PERFORM one ON COMMIT.
  PERFORM back ON ROLLBACK.
  WRITE / 'commit'.
  COMMIT WORK.
  WRITE / 'again'.
  COMMIT WORK.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("commit\none\ntwo\nlate\nagain");
  });

  it("ROLLBACK WORK runs PERFORM ON ROLLBACK and forgets PERFORM ON COMMIT", async () => {
    const code = `
FORM one.
  WRITE / 'one'.
ENDFORM.
FORM back.
  WRITE / 'back'.
ENDFORM.

START-OF-SELECTION.
  PERFORM one ON COMMIT.
  PERFORM back ON ROLLBACK.
  ROLLBACK WORK.
  COMMIT WORK.
  WRITE / 'done'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("back\ndone");
  });

  it("SET UPDATE TASK LOCAL", async () => {
    const code = `
SET UPDATE TASK LOCAL.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

});

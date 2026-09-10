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

  it("COMMIT WORK, sy-subrc is not changed", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      READ TABLE tab INDEX 1 TRANSPORTING NO FIELDS.
      COMMIT WORK.
      WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
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

});

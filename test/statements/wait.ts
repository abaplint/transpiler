import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

/** minimal fake database connection, tracks the changes of the open transaction */
class FakeConnection {
  public commits = 0;
  public pending: string[] = [];
  public committed: string[] = [];

  public async commit() {
    this.commits++;
    this.committed.push(...this.pending);
    this.pending = [];
  }
}

function fakeConnection(name: string) {
  const connection = new FakeConnection();
  abap.context.databaseConnections[name] = connection as any;
  return connection;
}

describe("Running statements - WAIT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("sets sy-subrc to zero when the condition is true", async () => {
    const code = `
DATA foo TYPE i.
foo = 1.
sy-subrc = 4.
WAIT UNTIL foo = 1 UP TO 1 SECONDS.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("times out and sets sy-subrc to eight", async () => {
    const code = `
DATA foo TYPE i.
WAIT UNTIL foo = 1 UP TO 1 SECONDS.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    const start = Date.now();
    await f(abap);
    const elapsed = Date.now() - start;

    expect(abap.console.get()).to.equal("8");
    expect(elapsed).to.be.lessThan(5000);
  });

  it("WAIT UP TO, sets sy-subrc to zero", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
READ TABLE tab INDEX 1 TRANSPORTING NO FIELDS.
WAIT UP TO 0 SECONDS.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("WAIT UP TO, commits the pending changes exactly once", async () => {
    const connection = fakeConnection("DEFAULT");
    connection.pending.push("row1");

    const js = await run("WAIT UP TO 0 SECONDS. WRITE 'after'.");
    const f = new AsyncFunction("abap", js);
    await f(abap);

    expect(connection.commits).to.equal(1);
    expect(connection.pending).to.deep.equal([]);
    expect(connection.committed).to.deep.equal(["row1"]);
    expect(abap.console.get()).to.equal("after");
  });

  it("WAIT UP TO, commits all open database connections", async () => {
    const first = fakeConnection("DEFAULT");
    const second = fakeConnection("MYCON");

    const js = await run("WAIT UP TO 0 SECONDS.");
    const f = new AsyncFunction("abap", js);
    await f(abap);

    expect(first.commits).to.equal(1);
    expect(second.commits).to.equal(1);
  });

  it("WAIT UNTIL, commits once when the execution is interrupted", async () => {
    const connection = fakeConnection("DEFAULT");
    connection.pending.push("row1");

    const code = `
DATA foo TYPE i.
WAIT UNTIL foo = 1 UP TO 1 SECONDS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);

    // the condition is polled repeatedly, but the LUW is only ended once
    expect(connection.commits).to.equal(1);
    expect(connection.committed).to.deep.equal(["row1"]);
  });

  it("WAIT UNTIL, does not commit when the condition is initially true", async () => {
    const connection = fakeConnection("DEFAULT");
    connection.pending.push("row1");

    const code = `
DATA foo TYPE i.
foo = 1.
WAIT UNTIL foo = 1 UP TO 1 SECONDS.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);

    expect(connection.commits).to.equal(0);
    expect(connection.pending).to.deep.equal(["row1"]);
    expect(abap.console.get()).to.equal("0");
  });

});

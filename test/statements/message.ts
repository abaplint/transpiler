import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {SQLiteDatabaseClient} from "../../packages/database-sqlite/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_message.prog.abap", contents}], {skipVersionCheck});
}

describe("Running statements - MESSAGE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("MESSAGE INTO", async () => {
    const code = `
    DATA lv_text TYPE string.
    MESSAGE e001(00) WITH 'foo' 'bar' INTO lv_text.
    WRITE / sy-msgty.
    WRITE / sy-msgid.
    WRITE / sy-msgno.
    WRITE / lv_text.`;

    abap.context.databaseConnections["DEFAULT"] = new SQLiteDatabaseClient();
    await abap.context.databaseConnections["DEFAULT"].connect();
    await abap.context.databaseConnections["DEFAULT"].execute(`
    CREATE TABLE t100 (sprsl NCHAR(1), arbgb NCHAR(20), msgnr NCHAR(3), text NCHAR(73), PRIMARY KEY(sprsl,arbgb,msgnr));
    INSERT INTO t100 VALUES ('E', '00', '001', '&1&2&3&4');`);

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("E\n00                  \n001\nfoobar");
  });

  it("MESSAGE fallback, no database initialized", async () => {
    const code = `
    DATA lv_text TYPE string.
    MESSAGE e123(abc) WITH 'foo' 'bar' INTO lv_text.
    WRITE / sy-msgid.
    WRITE / sy-msgno.
    WRITE / lv_text.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABC                 \n123\nE:ABC:123 foo bar");
  });

  it("MESSAGE empty msgid", async () => {
    const code = `
  DATA result TYPE string.
  CLEAR sy-msgid.
  CLEAR sy-msgno.
  MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno INTO result WITH 'moo'.
  WRITE result.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("I::000 moo");
  });

  it("MESSAGE, MessageNumber", async () => {
    const code = `
  DATA result TYPE string.
  MESSAGE ID 'ZFOO' TYPE 'I' NUMBER 100 INTO result WITH 'moo'.
  WRITE result.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("I:ZFOO:100 moo");
  });

  it("MESSAGE fallback, WITH integer", async () => {
    const code = `
    DATA lv_text TYPE string.
    DATA lv_int TYPE i.
    lv_int = 444.
    MESSAGE e123(abc) WITH lv_int INTO lv_text.
    WRITE / lv_text.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.include("444");
  });

  it("MESSAGE constant", async () => {
    const code = `MESSAGE 'hello' TYPE 'S'.`;

    const js = await run(code);
    new AsyncFunction("abap", js);
// hmm, this currently writes to console
  });

  it("MESSAGE with complex", async () => {
    const code = `MESSAGE s001(abc) WITH condense( |{ 'hello' }| ).`;

    const js = await run(code);
    new AsyncFunction("abap", js);
// hmm, this currently writes to console
  });

  it("MESSAGE with string", async () => {
    const code = `MESSAGE |hello| TYPE 'S'.`;

    const js = await run(code);
    console.dir(js);
    new AsyncFunction("abap", js);
// hmm, this currently writes to console
  });

});
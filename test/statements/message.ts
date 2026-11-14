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
  });

  it("MESSAGE with complex", async () => {
    const code = `MESSAGE s001(abc) WITH condense( |{ 'hello' }| ).`;

    const js = await run(code);
    new AsyncFunction("abap", js);
  });

  it("MESSAGE with string", async () => {
    const code = `MESSAGE |hello| TYPE 'S'.`;

    const js = await run(code);
    new AsyncFunction("abap", js);
  });

  it("MESSAGE with a type that does not exist ends the program", async () => {
    const code = `
DATA lv_type TYPE c LENGTH 1 VALUE 'Q'.
WRITE / 'before'.
MESSAGE ID '00' TYPE lv_type NUMBER '001'.
WRITE / 'after'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    let error = "";
    try {
      await f(abap);
    } catch (e) {
      error = (e as Error).message;
    }
    expect(error).to.contain("MESSAGE_TYPE_UNKNOWN");
    expect(abap.console.get()).to.equal("before");
  });

  it("MESSAGE oref, IF_T100_MESSAGE gives the message and its placeholders", async () => {
    const code = `
INTERFACE if_message.
  METHODS get_text RETURNING VALUE(result) TYPE string.
ENDINTERFACE.

INTERFACE if_t100_message.
  INTERFACES if_message.
  DATA: BEGIN OF t100key,
          msgid TYPE c LENGTH 20,
          msgno TYPE n LENGTH 3,
          attr1 TYPE c LENGTH 255,
          attr2 TYPE c LENGTH 255,
          attr3 TYPE c LENGTH 255,
          attr4 TYPE c LENGTH 255,
        END OF t100key.
ENDINTERFACE.

CLASS lcl_msg DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_t100_message.
    DATA mv_first TYPE c LENGTH 10.
    METHODS constructor.
ENDCLASS.

CLASS lcl_msg IMPLEMENTATION.
  METHOD constructor.
    mv_first = 'Alpha'.
    if_t100_message~t100key-msgid = 'ZZ'.
    if_t100_message~t100key-msgno = '001'.
    if_t100_message~t100key-attr1 = 'MV_FIRST'.
    if_t100_message~t100key-attr2 = 'NO_SUCH'.
  ENDMETHOD.
  METHOD if_message~get_text.
    result = 'not used'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_msg TYPE REF TO lcl_msg.
  CREATE OBJECT lo_msg.
  MESSAGE lo_msg TYPE 'S'.
  WRITE / sy-msgid.
  WRITE / sy-msgno.
  WRITE / sy-msgty.
  WRITE / sy-msgv1.
  WRITE / sy-msgv2.`;

    abap.context.databaseConnections["DEFAULT"] = new SQLiteDatabaseClient();
    await abap.context.databaseConnections["DEFAULT"].connect();
    await abap.context.databaseConnections["DEFAULT"].execute(`
    CREATE TABLE t100 (sprsl NCHAR(1), arbgb NCHAR(20), msgnr NCHAR(3), text NCHAR(73), PRIMARY KEY(sprsl,arbgb,msgnr));
    INSERT INTO t100 VALUES ('E', 'ZZ', '001', 'first &1 second &2');`);

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    const lines = abap.console.get().split("\n").map(l => l.trimEnd());
    expect(lines).to.deep.equal(["ZZ", "001", "S", "Alpha", "&NO_SUCH&"]);
  });

});
import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - MESSAGE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("MESSAGE INTO", async () => {
    const code = `
    DATA lv_text TYPE string.
    MESSAGE e001(00) WITH 'foo' 'bar' INTO lv_text.
    WRITE / sy-msgid.
    WRITE / sy-msgno.
    WRITE / lv_text.`;

    await abap.initDB(`
    CREATE TABLE t100 (sprsl NCHAR(1), arbgb NCHAR(20), msgnr NCHAR(3), text NCHAR(73), PRIMARY KEY(sprsl,arbgb,msgnr));
    INSERT INTO t100 VALUES ('E', '00', '001', '&1&2&3&4');`);

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("00\n001\nfoobar");
  });

  it("MESSAGE fallback, no database initialized", async () => {
    const code = `
    DATA lv_text TYPE string.
    MESSAGE e123(abc) WITH 'foo' 'bar' INTO lv_text.
    WRITE / sy-msgid.
    WRITE / sy-msgno.
    WRITE / lv_text.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("ABC\n123\nABC:123 foo bar");
  });

});
import {expect} from "chai";
import {ABAP, MemoryConsole, MemoryDataset} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;
// the runtime's own in-memory host: the file system the runtime asks for
// nothing but bytes
let files: {[name: string]: Uint8Array};

const cxroot = `
CLASS cx_root DEFINITION PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.`;

const cx = (name: string) => `
CLASS ${name} DEFINITION PUBLIC INHERITING FROM cx_root.
ENDCLASS.
CLASS ${name} IMPLEMENTATION.
ENDCLASS.`;

async function run(contents: string) {
  // the program first: runFiles evaluates the first object only, so the
  // exception classes are in the registry for the syntax check and stand in
  // as plain JavaScript classes at run time
  const js = await runFiles(abap, [
    {filename: "zfoobar.prog.abap", contents},
    {filename: "cx_root.clas.abap", contents: cxroot},
    {filename: "cx_sy_file_open_mode.clas.abap", contents: cx("cx_sy_file_open_mode")},
    {filename: "cx_sy_file_open.clas.abap", contents: cx("cx_sy_file_open")}]);
  abap.Classes["CX_ROOT"] = class CxRoot {};
  abap.Classes["CX_SY_FILE_OPEN_MODE"] = class CxSyFileOpenMode extends abap.Classes["CX_ROOT"] {};
  abap.Classes["CX_SY_FILE_OPEN"] = class CxSyFileOpen extends abap.Classes["CX_ROOT"] {};
  const f = new AsyncFunction("abap", js);
  await f(abap);
  return abap.console.get();
}

const hex = (bytes: Uint8Array) => Array.from(bytes).map(b => b.toString(16).toUpperCase().padStart(2, "0")).join("");
const bytesOf = (text: string) => new TextEncoder().encode(text);

// Every expectation below was measured on an SAP system (7.5x, Unicode, Linux)
// with the same statements, 2026-09-30.
describe("Running statements - DATASET", () => {

  beforeEach(async () => {
    const dataset = new MemoryDataset();
    files = dataset.files;
    abap = new ABAP({console: new MemoryConsole(), dataset});
  });

  it("without a host, OPEN DATASET is not supported", async () => {
    abap = new ABAP({console: new MemoryConsole()});
    let message = "";
    try {
      await run(`OPEN DATASET 'f' FOR INPUT IN BINARY MODE.`);
    } catch (e) {
      message = (e as Error).message;
    }
    expect(message).to.contain("not supported");
  });

  it("TEXT MODE writes UTF-8 lines: C without its trailing blanks, a string with them", async () => {
    await run(`
DATA lv_c TYPE c LENGTH 10.
DATA lv_s TYPE string.
lv_c = 'ab'.
lv_s = |cd |.
OPEN DATASET 'f' FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
TRANSFER lv_c TO 'f'.
TRANSFER lv_s TO 'f'.
CLOSE DATASET 'f'.`);
    expect(hex(files["f"])).to.equal("61620A6364200A");
  });

  it("TEXT MODE, LENGTH and NO END OF LINE", async () => {
    await run(`
DATA lv_6 TYPE c LENGTH 6 VALUE 'abcdef'.
DATA lv_2 TYPE c LENGTH 2 VALUE 'xy'.
DATA lv_1 TYPE c LENGTH 1 VALUE 'z'.
OPEN DATASET 'f' FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
TRANSFER lv_6 TO 'f' LENGTH 3.
TRANSFER lv_2 TO 'f' NO END OF LINE.
TRANSFER lv_1 TO 'f'.
CLOSE DATASET 'f'.`);
    expect(hex(files["f"])).to.equal("6162630A78797A0A");
  });

  it("TEXT MODE reads a line per READ, the last without LF, then sy-subrc 4 and the target cleared", async () => {
    files["f"] = bytesOf("a\nb");
    const out = await run(`
DATA lv_s TYPE string.
DATA lv_len TYPE i.
DATA lv_pos TYPE i.
DATA lv_f TYPE string VALUE 'f'.
OPEN DATASET 'f' FOR INPUT IN TEXT MODE ENCODING UTF-8.
DO 3 TIMES.
  lv_s = 'zz'.
  READ DATASET 'f' INTO lv_s ACTUAL LENGTH lv_len.
  WRITE / sy-subrc.
  GET DATASET lv_f POSITION lv_pos.
  WRITE: / lv_s, / lv_len, / lv_pos.
ENDDO.
CLOSE DATASET 'f'.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["0", "a", "1", "2", "0", "b", "1", "3", "4", "", "0", "3"]);
  });

  it("TEXT MODE, ACTUAL LENGTH is the whole line when the field is shorter, and a CR stays", async () => {
    files["f"] = bytesOf("longer line\na\r\n");
    const out = await run(`
DATA lv_c TYPE c LENGTH 5.
DATA lv_len TYPE i.
OPEN DATASET 'f' FOR INPUT IN TEXT MODE ENCODING UTF-8.
READ DATASET 'f' INTO lv_c ACTUAL LENGTH lv_len.
WRITE: / lv_c, / lv_len.
READ DATASET 'f' INTO lv_c ACTUAL LENGTH lv_len.
WRITE: / lv_len.
CLOSE DATASET 'f'.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["longe", "11", "2"]);
  });

  it("TEXT MODE, ENCODING DEFAULT is UTF-8", async () => {
    files["f"] = new Uint8Array([0xC3, 0xA4, 0xE2, 0x82, 0xAC, 0x0A]);
    const out = await run(`
DATA lv_s TYPE string.
DATA lv_len TYPE i.
OPEN DATASET 'f' FOR INPUT IN TEXT MODE ENCODING DEFAULT.
READ DATASET 'f' INTO lv_s ACTUAL LENGTH lv_len.
WRITE lv_len.
CLOSE DATASET 'f'.`);
    expect(out.trim()).to.equal("2");
  });

  it("BINARY MODE reads a fixed field short at the end: sy-subrc 4, padded, ACTUAL LENGTH in bytes", async () => {
    files["f"] = new Uint8Array([0x00, 0xFF, 0x0D, 0x0A, 0x41]);
    const out = await run(`
DATA lv_x TYPE x LENGTH 3.
DATA lv_len TYPE i.
OPEN DATASET 'f' FOR INPUT IN BINARY MODE.
DO 3 TIMES.
  READ DATASET 'f' INTO lv_x ACTUAL LENGTH lv_len.
  WRITE: / sy-subrc, / lv_x, / lv_len.
ENDDO.
CLOSE DATASET 'f'.
OPEN DATASET 'f' FOR INPUT IN BINARY MODE.
READ DATASET 'f' INTO lv_x MAXIMUM LENGTH 2 ACTUAL LENGTH lv_len.
WRITE: / sy-subrc, / lv_x, / lv_len.
CLOSE DATASET 'f'.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal([
      "0", "00FF0D", "3",
      "4", "0A4100", "2",
      "4", "000000", "0",
      "0", "00FF00", "2"]);
  });

  it("BINARY MODE, a C field and a string are UTF-16LE, the C field at full length", async () => {
    const out = await run(`
DATA lv_c TYPE c LENGTH 4.
DATA lv_s TYPE string.
DATA lv_len TYPE i.
lv_c = 'ab'.
lv_s = |cd |.
OPEN DATASET 'f' FOR OUTPUT IN BINARY MODE.
TRANSFER lv_c TO 'f'.
TRANSFER lv_s TO 'f'.
CLOSE DATASET 'f'.
OPEN DATASET 'f' FOR INPUT IN BINARY MODE.
READ DATASET 'f' INTO lv_s ACTUAL LENGTH lv_len.
WRITE: / strlen( lv_s ), / lv_len.
CLOSE DATASET 'f'.`);
    expect(hex(files["f"])).to.equal("6100620020002000630064002000");
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["7", "14"]);
  });

  it("BINARY MODE, an xstring round trip", async () => {
    const out = await run(`
DATA lv_x TYPE xstring.
lv_x = '00FF0D0A41'.
OPEN DATASET 'f' FOR OUTPUT IN BINARY MODE.
TRANSFER lv_x TO 'f'.
CLOSE DATASET 'f'.
CLEAR lv_x.
OPEN DATASET 'f' FOR INPUT IN BINARY MODE.
READ DATASET 'f' INTO lv_x.
WRITE: / sy-subrc, / lv_x.
READ DATASET 'f' INTO lv_x.
WRITE: / sy-subrc.
CLOSE DATASET 'f'.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["0", "00FF0D0A41", "4"]);
  });

  it("OPEN of a missing file FOR INPUT is sy-subrc 8 with the MESSAGE", async () => {
    const out = await run(`
DATA lv_msg TYPE string.
OPEN DATASET 'nope' FOR INPUT IN BINARY MODE MESSAGE lv_msg.
WRITE: / sy-subrc, / lv_msg.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["8", "No such file or directory"]);
  });

  it("FOR APPENDING appends, FOR UPDATE overwrites in place, FOR OUTPUT truncates", async () => {
    files["a"] = bytesOf("x\n");
    files["u"] = bytesOf("12345");
    files["o"] = bytesOf("12345");
    await run(`
DATA lv_x TYPE xstring.
OPEN DATASET 'a' FOR APPENDING IN TEXT MODE ENCODING UTF-8.
TRANSFER 'y' TO 'a'.
CLOSE DATASET 'a'.
lv_x = '58'.
OPEN DATASET 'u' FOR UPDATE IN BINARY MODE.
TRANSFER lv_x TO 'u'.
CLOSE DATASET 'u'.
lv_x = '59'.
OPEN DATASET 'o' FOR OUTPUT IN BINARY MODE.
TRANSFER lv_x TO 'o'.
CLOSE DATASET 'o'.`);
    expect(new TextDecoder().decode(files["a"])).to.equal("x\ny\n");
    expect(hex(files["u"])).to.equal("5832333435");
    expect(hex(files["o"])).to.equal("59");
  });

  it("SET DATASET POSITION, and END OF FILE", async () => {
    files["f"] = bytesOf("a\nb");
    const out = await run(`
DATA lv_s TYPE string.
DATA lv_pos TYPE i.
DATA lv_f TYPE string VALUE 'f'.
OPEN DATASET 'f' FOR INPUT IN TEXT MODE ENCODING UTF-8.
READ DATASET 'f' INTO lv_s.
SET DATASET 'f' POSITION 0.
READ DATASET 'f' INTO lv_s.
WRITE: / sy-subrc, / lv_s.
SET DATASET 'f' POSITION END OF FILE.
GET DATASET lv_f POSITION lv_pos.
READ DATASET 'f' INTO lv_s.
WRITE: / lv_pos, / sy-subrc.
CLOSE DATASET 'f'.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["0", "a", "3", "4"]);
  });

  it("READ from a file opened FOR OUTPUT is sy-subrc 4; CLOSE of a closed file and DELETE of a missing one", async () => {
    const out = await run(`
DATA lv_s TYPE string.
OPEN DATASET 'f' FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
READ DATASET 'f' INTO lv_s.
WRITE / sy-subrc.
CLOSE DATASET 'f'.
CLOSE DATASET 'f'.
WRITE / sy-subrc.
DELETE DATASET 'f'.
WRITE / sy-subrc.
DELETE DATASET 'f'.
WRITE / sy-subrc.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["4", "0", "0", "4"]);
  });

  it("TRANSFER to a file not open, or opened FOR INPUT, raises CX_SY_FILE_OPEN_MODE; OPEN twice raises CX_SY_FILE_OPEN", async () => {
    files["f"] = bytesOf("a\n");
    const out = await run(`
TRY.
    TRANSFER 'q' TO 'g'.
  CATCH cx_sy_file_open_mode.
    WRITE / 'not open'.
ENDTRY.
OPEN DATASET 'f' FOR INPUT IN TEXT MODE ENCODING UTF-8.
TRY.
    TRANSFER 'q' TO 'f'.
  CATCH cx_sy_file_open_mode.
    WRITE / 'input'.
ENDTRY.
TRY.
    OPEN DATASET 'f' FOR INPUT IN TEXT MODE ENCODING UTF-8.
  CATCH cx_sy_file_open.
    WRITE / 'twice'.
ENDTRY.
CLOSE DATASET 'f'.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["not open", "input", "twice"]);
    expect(files["g"]).to.equal(undefined);
  });

  it("an addition is a keyword of the statement, never the text of an operand", async () => {
    await run(`
DATA lv_type TYPE string VALUE 'f'.
DATA lv_filter TYPE string VALUE 'no end of line'.
OPEN DATASET lv_type FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
TRANSFER lv_filter TO lv_type.
TRANSFER 'no end of line' TO lv_type.
CLOSE DATASET lv_type.`);
    expect(new TextDecoder().decode(files["f"])).to.equal("no end of line\nno end of line\n");
  });

  it("READ from a file never opened raises CX_SY_FILE_OPEN_MODE", async () => {
    const out = await run(`
DATA lv_s TYPE string.
TRY.
    READ DATASET 'f' INTO lv_s.
  CATCH cx_sy_file_open_mode.
    WRITE / 'not open'.
ENDTRY.`);
    expect(out.trim()).to.equal("not open");
  });

  it("BINARY MODE, a C field takes two bytes per character", async () => {
    files["f"] = new Uint8Array([0x61, 0x00, 0x62, 0x00, 0x63, 0x00, 0x0A, 0x00]);
    const out = await run(`
DATA lv_c TYPE c LENGTH 3.
DATA lv_len TYPE i.
OPEN DATASET 'f' FOR INPUT IN BINARY MODE.
READ DATASET 'f' INTO lv_c ACTUAL LENGTH lv_len.
WRITE: / sy-subrc, / lv_c, / lv_len.
CLOSE DATASET 'f'.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["0", "abc", "6"]);
  });

  it("DELETE of an open file answers 0 and closes it", async () => {
    files["f"] = bytesOf("a\n");
    const out = await run(`
DATA lv_s TYPE string.
OPEN DATASET 'f' FOR INPUT IN TEXT MODE ENCODING UTF-8.
DELETE DATASET 'f'.
WRITE / sy-subrc.
TRY.
    READ DATASET 'f' INTO lv_s.
  CATCH cx_sy_file_open_mode.
    WRITE / 'closed'.
ENDTRY.
OPEN DATASET 'f' FOR INPUT IN TEXT MODE ENCODING UTF-8.
WRITE / sy-subrc.`);
    expect(out.split("\n").map(l => l.trim())).to.deep.equal(["0", "closed", "8"]);
  });

  it("NON-UNICODE, a byte-order mark and CODE PAGE are refused by name", async () => {
    for (const addition of ["IN TEXT MODE ENCODING NON-UNICODE", "IN TEXT MODE ENCODING UTF-8 WITH BYTE-ORDER MARK",
      "IN TEXT MODE ENCODING UTF-8 SKIPPING BYTE-ORDER MARK", "IN LEGACY TEXT MODE CODE PAGE '1100'"]) {
      let message = "";
      try {
        await run(`OPEN DATASET 'f' FOR INPUT ${addition}.`);
      } catch (e) {
        message = (e as Error).message;
      }
      expect(message, addition).to.contain("not supported");
    }
  });

});

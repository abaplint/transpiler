import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - REPLACE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("REPLACE ALL", async () => {
    const code = `
  DATA str TYPE string.
  str = 'aabbccbb'.
  REPLACE ALL OCCURRENCES OF |bb| IN str WITH |dd|.
  ASSERT str = 'aaddccdd'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, dump/throw exception", async () => {
    const code = `
  DATA lv_string TYPE string.
  lv_string = 'foobar'.
  REPLACE ALL OCCURRENCES OF || IN lv_string WITH 'sdfs'.
  ASSERT lv_string = 'foobar'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e).to.contain("REPLACE, zero length input");
    }
  });

  it("REPLACE identical contents", async () => {
    const code = `
  DATA lv_string TYPE string.
  lv_string = 'ab'.
  REPLACE ALL OCCURRENCES OF 'ab' IN lv_string WITH \`ab\`.
  ASSERT lv_string = 'ab'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE FIRST", async () => {
    const code = `
  DATA lv_host TYPE string.
  lv_host = 'abc'.
  REPLACE FIRST OCCURRENCE OF '' IN lv_host WITH ''.
  ASSERT lv_host = 'abc'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE IN SECTION LENGTH", async () => {
    const code = `
  DATA lv_value TYPE string.
  lv_value = 'abfooab'.
  REPLACE REGEX 'ab' IN SECTION LENGTH 2 OF lv_value WITH ''.
  WRITE lv_value.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("fooab");
  });

  it("REPLACE, regex not found", async () => {
    const code = `
    DATA lv_var TYPE string.
    lv_var = '1'.
    REPLACE REGEX 'sdfds' IN lv_var WITH ''.
    WRITE sy-subrc.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("REPLACE, regex found", async () => {
    const code = `
    DATA lv_var TYPE string.
    lv_var = 'sdfds'.
    REPLACE REGEX 'sdfds' IN lv_var WITH ''.
    WRITE sy-subrc.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("REPLACE, not found, should set subrc", async () => {
    const code = `
    DATA foo TYPE string.
    foo = 'abc'.
    REPLACE FIRST OCCURRENCE OF '1' IN foo WITH \` \`.
    ASSERT sy-subrc = 4.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, found, should set subrc", async () => {
    const code = `
    DATA foo TYPE string.
    foo = 'abc'.
    REPLACE FIRST OCCURRENCE OF 'a' IN foo WITH \` \`.
    ASSERT sy-subrc = 0.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, plus", async () => {
    const code = `
    DATA foo TYPE string.
    foo = 'a+c'.
    REPLACE ALL OCCURRENCES OF '+' IN foo WITH '_'.
    ASSERT foo = 'a_c'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, all regex", async () => {
    const code = `
  data str type string.
  str = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
  replace all occurrences of regex '[AEG]' in str with ''.
  write str.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("BCDFHIJKLMNOPQRSTUVWXYZ");
  });

  it("REPLACE newline", async () => {
    const code = `
  DATA lv_xml TYPE string.
  REPLACE ALL OCCURRENCES OF |\\n| IN lv_xml WITH ||.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, service", async () => {
    const code = `
    CONSTANTS lc_content_type TYPE string VALUE 'application/x-git-<service>-pack-advertisement'.
    DATA lv_expected_content_type TYPE string.
    lv_expected_content_type = lc_content_type.
    REPLACE '<service>' IN lv_expected_content_type WITH 'moo'.
    ASSERT lv_expected_content_type = 'application/x-git-moo-pack-advertisement'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, split in fives", async () => {
    const code = `
    DATA cipher_text TYPE string.
    cipher_text = |nrmwyoldrmtob|.
    REPLACE ALL OCCURRENCES OF REGEX \`.{5}\` IN cipher_text WITH |$& |.
    WRITE cipher_text.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("nrmwy oldrm tob");
  });

  it("REPLACE, ignoring case", async () => {
    const code = `
  DATA str TYPE string VALUE '%2Fsrc%2f'.
REPLACE ALL OCCURRENCES OF '%2F' IN str WITH '/' IGNORING CASE.
ASSERT str = '/src/'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, with escaped values", async () => {
    const code =
      "DATA string_to_parse TYPE string.\n" +
      "string_to_parse = 'Description first $values{@link cl_aff_test_types_for_writer.data:enum_values} Unused Text'.\n" +
      "REPLACE ALL OCCURRENCES OF REGEX `\\$values[\\s]*(:[\\s]*)?\\{[\\s]*@link` IN string_to_parse WITH `\\$values\\{@link`.\n" +
      "ASSERT string_to_parse = 'Description first $values{@link cl_aff_test_types_for_writer.data:enum_values} Unused Text'.";

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, all occur, no infinite loop", async () => {
    const code = `
DATA lv_pattern TYPE string.
lv_pattern = '#foo#'.
REPLACE ALL OCCURRENCES OF '#' IN lv_pattern WITH '##'.
WRITE lv_pattern.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("##foo##");
  });

  it("REPLACE, SECTION OFFSET LENGTH", async () => {
    const code = `
DATA val TYPE string.
val = 'bbbb'.
REPLACE SECTION OFFSET 1 LENGTH 2 OF val WITH 'aa'.
WRITE val.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("baab");
  });

  it("REPLACE, REGEX", async () => {
    const code = `
DATA lv_url TYPE string.
lv_url = |https://github.com/abapGit/abapGit.git|.
REPLACE REGEX '\\.git$' IN lv_url WITH space.
WRITE / lv_url.
WRITE / strlen( lv_url ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("https://github.com/abapGit/abapGit\n34");
  });

  it("REPLACE, IN TABLE", async () => {
    const code = `
TYPES char20 TYPE c LENGTH 20.
DATA tab TYPE STANDARD TABLE OF char20 WITH DEFAULT KEY.
REPLACE ALL OCCURRENCES OF 'foo' IN TABLE tab WITH 'bar'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, SECTION OFFSET, long", async () => {
    const code = `
DATA lv_input TYPE string.
lv_input = '0123456789'.
REPLACE SECTION OFFSET 2 LENGTH 4 OF lv_input WITH 'a'.
WRITE lv_input.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("01a6789");
  });

  it("REPLACE, SECTION OFFSET, long, space", async () => {
    const code = `
DATA lv_input TYPE string.
lv_input = '0123456789'.
REPLACE SECTION OFFSET 2 LENGTH 4 OF lv_input WITH 'a '.
WRITE lv_input.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("01a6789");
  });

  it("REPLACE, OF char with spaces", async () => {
    const code = `
DATA foo TYPE c LENGTH 10.
DATA str TYPE string.
foo = 'abc'.
str = 'abcde'.
REPLACE FIRST OCCURRENCE OF foo IN str WITH |hello|.
ASSERT sy-subrc = 0.
WRITE str.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hellode");
  });

  it("REPLACE, pcre", async () => {
    const code = `
    DATA default TYPE string.
    default = 'footbar'.
    REPLACE PCRE \`T|t\` IN default WITH \` \`.
    ASSERT default = 'foo bar'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, check subrc, REPLACEMENT LENGTH", async () => {
    const code = `
DATA sdummy TYPE string.
DATA match TYPE i.
sdummy = '0894ef45'.
REPLACE FIRST OCCURRENCE OF REGEX '^0894ef45$' IN sdummy WITH 'abc' REPLACEMENT LENGTH match IGNORING CASE.
ASSERT sy-subrc = 0.
WRITE / match.
WRITE / sdummy.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("3\nabc");
  });

  it("REPLACE, check subrc, REPLACEMENT LENGTH", async () => {
    const code = `
DATA sdummy TYPE string.
DATA match TYPE i.
sdummy = '0894ef45aa'.
REPLACE FIRST OCCURRENCE OF REGEX '^0894ef45' IN sdummy WITH 'abc' REPLACEMENT LENGTH match IGNORING CASE.
ASSERT sy-subrc = 0.
WRITE / match.
WRITE / sdummy.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("3\nabcaa");
  });

  it("REPLACE, regex case?", async () => {
    const code = `
DATA sdummy TYPE string.
DATA match TYPE i.
sdummy = 'aabb'.
REPLACE FIRST OCCURRENCE OF REGEX '^([0-9A-F]{4})$'
  IN sdummy WITH 'foo' REPLACEMENT LENGTH match IGNORING CASE.
ASSERT sy-subrc = 0.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("REPLACE, check subrc, REPLACEMENT LENGTH, another test", async () => {
    const code = `
DATA sdummy TYPE string.
DATA match TYPE i.
sdummy = '0894ef45-77a9-1ed8-a495-bd69397619c0'.
REPLACE FIRST OCCURRENCE OF REGEX '^([0-9A-F]{8})-([0-9A-F]{4})-([0-9A-F]{4})-([0-9A-F]{4})-([0-9A-F]{12})$'
  IN sdummy WITH '$1$2$3$4$5' REPLACEMENT LENGTH match IGNORING CASE.
ASSERT sy-subrc = 0.
WRITE / match.
WRITE / sdummy.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("32\n0894ef4577a91ed8a495bd69397619c0");
  });

});
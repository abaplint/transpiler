import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - REPLACE", () => {

  beforeEach(async () => {
    abap = new ABAP();
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
  REPLACE ALL OCCURRENCES OF '' IN lv_string WITH 'sdfs'.
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

});
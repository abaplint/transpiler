import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - escape", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("escape() for html attribute", async () => {
    const code = `
      CONSTANTS e_html_attr TYPE i VALUE 5.
      DATA lv_result TYPE string.
      lv_result = escape( val = |abc123&<>"'| format = e_html_attr ).
      WRITE lv_result.
      `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(
      `abc123&amp;&lt;&gt;&quot;&#39;`);
  });

  it("escape() for html text", async () => {
    const code = `
      CONSTANTS e_html_text TYPE i VALUE 4.
      DATA lv_result TYPE string.
      lv_result = escape( val = |abc123&<>"'| format = e_html_text ).
      WRITE lv_result.
      `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(
      `abc123&amp;&lt;&gt;"'`);
  });

  it("escape() for url", async () => {
    const code = `
      CONSTANTS e_url TYPE i VALUE 12.
      DATA lv_result TYPE string.
      lv_result = escape( val = |abc123&<>"'| format = e_url ).
      WRITE lv_result.
      `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(
      `abc123&%3C%3E%22'`);
  });

  it("escape(), json1", async () => {
    const code = `
    CONSTANTS e_json_string TYPE i VALUE 24.
    DATA lv_result TYPE string.
    lv_result = escape(
      val    = |abc123&<>"'|
      format = e_json_string ).
    WRITE lv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`abc123&<>\\"'`);
  });

  it("escape(), json2", async () => {
    const code = `
    CONSTANTS e_json_string TYPE i VALUE 24.
    DATA lv_result TYPE string.
    lv_result = escape(
      val    = '{"foo": 0}'
      format = e_json_string ).
    WRITE lv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`{\\"foo\\": 0}`);
  });

  it("escape(), json3", async () => {
    const code = `
    CONSTANTS e_json_string TYPE i VALUE 24.
    DATA lv_result TYPE string.
    lv_result = escape(
      val    = |\\n|
      format = e_json_string ).
    ASSERT lv_result = |\\\\n|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escape(), xml attr", async () => {
    const code = `
    CONSTANTS e_xml_attr TYPE i VALUE 1.
    DATA lv_result TYPE string.
    lv_result = escape(
      val    = |abc123&<>"'|
      format = e_xml_attr ).
    WRITE lv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`abc123&amp;&lt;>&quot;&apos;`);
  });

  it("escape(), js, quote", async () => {
    const code = `
    DATA escaped TYPE string.
    escaped = escape( val = |"| format = 8 ).
    ASSERT escaped = '\\"'.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escape(), js, ping", async () => {
    const code = `
    DATA escaped TYPE string.
    escaped = escape( val = |'| format = 8 ).
    ASSERT escaped = |\\\\'|.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});

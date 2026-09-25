import {expect} from "chai";
import {ABAP} from "../src";

describe("escape, e_json_string", () => {
  const abap = new ABAP();
  const escape = (val: string): string => abap.builtin.escape({val, format: 24}).get();

  it("every control character below U+0020", () => {
    const all = Array.from({length: 32}, (_, i) => escape(String.fromCharCode(i)));
    expect(all.join(",")).to.equal(
      "\\u0000,\\u0001,\\u0002,\\u0003,\\u0004,\\u0005,\\u0006,\\u0007,\\b,\\t,\\n,\\u000B,\\f,\\r,\\u000E,\\u000F," +
      "\\u0010,\\u0011,\\u0012,\\u0013,\\u0014,\\u0015,\\u0016,\\u0017,\\u0018,\\u0019,\\u001A,\\u001B,\\u001C,\\u001D,\\u001E,\\u001F");
  });

  it("the result is valid JSON", () => {
    let all = "";
    for (let i = 0; i < 128; i++) {
      all += String.fromCharCode(i);
    }
    expect(JSON.parse("\"" + escape(all) + "\"")).to.equal(all);
  });

  it("U+007F, slash, apostrophe and non-ASCII are kept", () => {
    const val = "\u007F/'\u00E9\u20AC\u2028";
    expect(escape(val)).to.equal(val);
  });
});

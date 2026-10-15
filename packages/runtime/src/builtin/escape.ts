import {Character, FieldSymbol, String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {position} from "./_position";

const JSON_SHORT: {[char: string]: string} = {
  "\\": "\\\\",
  "\"": "\\\"",
  "\b": "\\b",
  "\t": "\\t",
  "\n": "\\n",
  "\f": "\\f",
  "\r": "\\r",
};

// backslash, quote, and \b \t \n \f \r by their short forms; the other control
// characters below U+0020 as \u00XX in upper case hex; U+007F and non-ASCII unchanged
function escapeJsonCharacter(char: string): string {
  return JSON_SHORT[char] ?? "\\u" + char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0");
}

// the characters below 0x80 each URL format keeps, all others below 0x80 become %hh
const URL_KEEP_ALWAYS = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
const URL_KEPT: {[format: number]: string} = {
  12: URL_KEEP_ALWAYS + "!$'()*+,-._&/:;=?@",
  14: URL_KEEP_ALWAYS + "!$'()*+,-._",
  16: URL_KEEP_ALWAYS + "!$'()*+,-._&/:;=?@~#[]",
  18: URL_KEEP_ALWAYS + "-._~",
};

function escapeUrl(val: string, kept: string): string {
  let ret = "";
  for (const char of val) {
    if (char.charCodeAt(0) < 0x80 && kept.includes(char) === false) {
      ret += "%" + char.charCodeAt(0).toString(16).toUpperCase().padStart(2, "0");
    } else {
      ret += char;
    }
  }
  return ret;
}

export function escape(input: {val: ICharacter | FieldSymbol | string, format: INumeric | number }): String {
  let source: ICharacter | string;
  if (input.val instanceof FieldSymbol) {
    const pointer = input.val.getPointer() as ICharacter | undefined;
    if (pointer === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    source = pointer;
  } else {
    source = input.val;
  }

  // a c operand counts without its trailing blanks, as in the other string functions
  let val = typeof source === "string" ? source
    : source instanceof Character ? source.getTrimEnd() : source.get();
  const format = position(input.format)!;

// todo, optimize/cache regexes
  switch (format) {
    case 0: // e_xml_text: & and < only, as in SAP; >, quotes and control characters stay
      val = val.replace(/&/g, "&amp;");
      val = val.replace(/</g, "&lt;");
      break;
    case 1: // e_xml_attr
      val = val.replace(/&/g, "&amp;");
      val = val.replace(/</g, "&lt;");
      val = val.replace(/"/g, "&quot;");
      val = val.replace(/'/g, "&apos;");
      break;
    case 4: // e_html_text
      val = val.replace(/&/g, "&amp;");
      val = val.replace(/</g, "&lt;");
      val = val.replace(/>/g, "&gt;");
      break;
    case 5: // e_html_attr
      val = val.replace(/&/g, "&amp;");
      val = val.replace(/</g, "&lt;");
      val = val.replace(/>/g, "&gt;");
      val = val.replace(/"/g, "&quot;");
      val = val.replace(/'/g, "&#39;");
      break;
    case 12: // e_url
    case 14: // e_url_full
    case 16: // e_uri
    case 18: // e_uri_full
      val = escapeUrl(val, URL_KEPT[format]);
      break;
    case 8: // e_html_js
      val = val.replace(/"/g, "\\\"");
      val = val.replace(/'/g, "\\'");
      break;
    case 24: // e_json_string
      val = val.replace(/[\\"\u0000-\u001F]/g, escapeJsonCharacter);
      break;
    default:
// todo, runtime error
  }

  return new String().set(val);
}

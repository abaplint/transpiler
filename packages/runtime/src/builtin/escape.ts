/* eslint-disable @typescript-eslint/ban-types */
import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function escape(input: {val: ICharacter | string, format: INumeric | number }): String {

  let val = typeof input.val === "string" ? input.val : input.val.get();
  const format = typeof input.format === "number" ? input.format : input.format.get();

// todo, optimize/cache regexes
  switch (format) {
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
      val = encodeURI(val);
      break;
    case 8: // e_html_js
      val = val.replace(/"/g, "\\\"");
      val = val.replace(/'/g, "\\'");
      break;
    case 24: // e_json_string
      val = val.replace(/"/g, "\\\"");
      val = val.replace(/\n/g, "\\n");
      break;
    default:
// todo, runtime error
  }

  return new String().set(val);
}
import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

type options = {
  timestamp?: string,
  date?: string,
  time?: string,
  width?: number,
};

export function templateFormatting(source: ICharacter | INumeric, options: options) {
  let text = source.get() + "";
  if (options.timestamp === "iso") {
    text = text.substr(0,4) + "-" + text.substr(4,2) + "-" + text.substr(6,2) + "T" + text.substr(8,2) + ":" + text.substr(10,2) + ":" + text.substr(12,2);
  }
  if (options.date === "iso") {
    text = text.substr(0,4) + "-" + text.substr(4,2) + "-" + text.substr(6,2);
  }
  if (options.time === "iso") {
    text = text.substr(0,2) + ":" + text.substr(2,2) + ":" + text.substr(4,2);
  }
  if (options.width) {
    text = text.padEnd(options.width, " ");
  }
  return text;
}
import {templateFormatting} from "../template_formatting";
import {Character} from "../types";
import {ICharacter} from "../types/_character";

export interface IConvertSource {
  date: ICharacter | string,
  time: ICharacter | string,
  zone: ICharacter | string,
}

export interface IConvertTarget {
  stamp: ICharacter,
}

export function convert(source: IConvertSource, target: IConvertTarget) {
  let str = "";

  if (typeof source.date === "string") {
    str += source.date;
  } else {
    str += source.date.get();
  }

  if (typeof source.time === "string") {
    str += source.time;
  } else {
    str += source.time.get();
  }

  str = templateFormatting(new Character({length: 14}).set(str), {timestamp: "iso"});
  str += "Z";

  const t1 = new Date(Date.parse(str));
  const out = t1.toISOString().slice(0, 19).replace(/-/g, "").replace(/:/g, "").replace("T", "");

  target.stamp.set(out);
}
import {templateFormatting} from "../template_formatting";
import {Character} from "../types";
import {ICharacter} from "../types/_character";

export interface IConvertSource {
  date?: ICharacter | string,
  time?: ICharacter | string,
  stamp?: ICharacter | string,
  zone: ICharacter | string,
}

export interface IConvertTarget {
  stamp?: ICharacter,
  date?: ICharacter,
  time?: ICharacter,
}

export function convert(source: IConvertSource, target: IConvertTarget) {
  let str = "";

  if (source.date) {
    if (typeof source.date === "string") {
      str += source.date;
    } else {
      str += source.date.get();
    }
  }

  if (source.time) {
    if (typeof source.time === "string") {
      str += source.time;
    } else {
      str += source.time.get();
    }
  }

  if (source.stamp) {
    if (typeof source.stamp === "string") {
      str += source.stamp;
    } else {
      str += source.stamp.get();
    }
  }

  str = templateFormatting(new Character({length: 14}).set(str), {timestamp: "iso"});
  str += "Z";

  if (str === "0000-00-00T00:00:00Z") {
    target.stamp?.clear();
    target.date?.clear();
    target.time?.clear();
    return;
  }

  const t1 = new Date(Date.parse(str));
  const out = t1.toISOString().slice(0, 19).replace(/-/g, "").replace(/:/g, "").replace("T", "");

  if (target.stamp) {
    target.stamp.set(out);
  }
  if (target.date) {
    target.date.set(out.substr(0,8)); // + out.substr(2,2) + out.substr(0,2));
  }
  if (target.time) {
    target.time.set(out.substring(8));
  }
}
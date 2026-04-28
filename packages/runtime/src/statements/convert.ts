import {ICharacter} from "../types/_character";
import {Temporal} from "temporal-polyfill";
import {INumeric} from "../types/_numeric";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IConvertSource {
  date?: ICharacter | string,
  time?: ICharacter | string,
  stamp?: ICharacter | INumeric | string,
  utclong?: ICharacter | string,
  zone: ICharacter | string,
}

export interface IConvertTarget {
  stamp?: ICharacter,
  date?: ICharacter,
  time?: ICharacter,
  utclong?: ICharacter,
}

export function convert(source: IConvertSource, target: IConvertTarget) {
  /*
  console.dir(source);
  console.dir(target);
  */
  let date = "";
  if (source.date) {
    if (typeof source.date === "string") {
      date = source.date;
    } else {
      date = source.date.get();
    }
    if (date.trimEnd() === "") {
      date = "00000000";
    }
  }

  let time = "";
  if (source.time) {
    if (typeof source.time === "string") {
      time = source.time;
    } else {
      time = source.time.get();
    }
    if (time.trimEnd() === "") {
      time = "000000";
    }
  }

  let stamp = "";
  if (source.stamp) {
    if (typeof source.stamp === "string") {
      stamp = source.stamp;
    } else {
      stamp = source.stamp.get() + "";
    }
  }

  let utclong = "";
  if (source.utclong) {
    if (typeof source.utclong === "string") {
      utclong = source.utclong;
    } else {
      utclong = source.utclong.get() + "";
    }
    utclong = utclong.trim();
  }

  let zone = "";
  if (source.zone) {
    if (typeof source.zone === "string") {
      zone = source.zone;
    } else {
      zone = source.zone.get() + "";
    }
    zone = zone.trimEnd();
  }
  let utcUsed = false;
  if (zone.trim() === "") {
    utcUsed = true;
    zone = "UTC";
  }

////////////////////////

  let zoned: Temporal.ZonedDateTime | undefined = undefined;
  if (utclong !== "") {
    if (utclong === "0000-00-00 00:00:00.0000000") {
      target.date?.clear();
      target.time?.clear();
      target.utclong?.clear();
      return;
    }
    const datePart = utclong.substring(0, 10);
    const timePart = utclong.substring(11, 19);
    const pt = Temporal.PlainTime.from(timePart);
    zoned = Temporal.PlainDate.from(datePart).toZonedDateTime({timeZone: "UTC", plainTime: pt});
    zoned = zoned.withTimeZone(zone);
  } else if (date !== "" && time !== "") {
    if (date === "00000000" && time === "000000") {
      target.stamp?.clear();
      return;
    }
    const pt = Temporal.PlainTime.from(time.substring(0, 2) + ":" + time.substring(2, 4) + ":" + time.substring(4, 6));
    zoned = Temporal.PlainDate.from(date).toZonedDateTime({timeZone: zone, plainTime: pt});
    zoned = zoned.withTimeZone("UTC");
  } else {
    if (stamp === "0") {
      target.date?.clear();
      target.time?.clear();
      return;
    }
    const pt = Temporal.PlainTime.from(stamp.substring(8, 10) + ":" + stamp.substring(10, 12) + ":" + stamp.substring(12, 14));
    zoned = Temporal.PlainDate.from(stamp.substring(0, 8)).toZonedDateTime({timeZone: "UTC", plainTime: pt});
    zoned = zoned.withTimeZone(zone);
  }

  const d = zoned.toPlainDate().toString().replace(/-/g, "");
  const t = zoned.toPlainTime().toString().replace(/:/g, "");

  if (target.stamp) {
    target.stamp.set(d + t);
  }
  if (target.date) {
    target.date.set(d);
  }
  if (target.time) {
    target.time.set(t);
  }
  if (target.utclong) {
    const targetDate = zoned.toPlainDate().toString();
    const targetTime = zoned.toPlainTime().toString().substring(0, 8);
    const fractionalSeconds = utclong !== "" ? utclong.substring(19) : ".0000000";
    target.utclong.set(targetDate + " " + targetTime + fractionalSeconds);
  }

  if (utcUsed) {
    abap.builtin.sy.get().subrc.set(4);
  } else {
    abap.builtin.sy.get().subrc.set(0);
  }

}
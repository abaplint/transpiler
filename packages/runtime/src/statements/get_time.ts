import {Packed, Structure} from "../types";
import {ICharacter} from "../types/_character";

type options = {
  field?: ICharacter,
  stamp?: ICharacter | Packed,
  sy?: Structure,
};

export function getTime(options?: options): void {

  const d = new Date();
  const date = d.getUTCFullYear() +
               (d.getUTCMonth() + 1 + "").padStart(2, "0") +
               (d.getUTCDate() + "").padStart(2, "0");
  const time = (d.getUTCHours() + "").padStart(2, "0") +
               (d.getUTCMinutes() + "").padStart(2, "0") +
               (d.getUTCSeconds() + "").padStart(2, "0");

  if (options === undefined) {
    options = {};
  }
  if (options?.sy === undefined) {
    // @ts-ignore
    options.sy = abap.builtin.sy;
  }

  options.sy!.get().datlo.set(date);
  options.sy!.get().datum.set(date);
  options.sy!.get().timlo.set(time);
  options.sy!.get().uzeit.set(time);

  if (options?.field) {
    options.field.set(time);
  }
  if (options?.stamp) {
    if (options.stamp instanceof Packed && options.stamp.getDecimals() === 7) {
      // set once, from the digits: a timestampl scaled to its 7 decimals has 21 digits,
      // adding the milliseconds via get() would go through a double and the last digits
      // would be noise - at a whole second even below it (20261014103400 -> 20261014103399.999…)
      const ms = (d.getUTCMilliseconds() + "").padStart(3, "0");
      options.stamp.set(date + time + "." + ms + "0000");
    } else {
      options.stamp.set(date + time);
    }
  }
}
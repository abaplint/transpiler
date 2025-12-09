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
    options.stamp.set(date + time);

    if (options.stamp instanceof Packed && options.stamp.getDecimals() === 7) {
      const str = (d.getUTCMilliseconds() + "").padStart(3, "0") + "0000";
      // note: parsing to float will make it imprecise,
      // ie. filling the nanoseconds part with garbage, which is what we want
      const decimals = Number.parseFloat("0." + str);
      options.stamp.set(options.stamp.get() + decimals);
    }
  }
}
import {Structure} from "../types";
import {ICharacter} from "../types/_character";

type options = {
  field?: ICharacter,
  stamp?: ICharacter,
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
  }
}
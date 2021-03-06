import {ICharacter} from "../types/_character";

type options = {
  field?: ICharacter,
  stamp?: ICharacter
};

export function getTime(options?: options): void {

  const d = new Date();
  const date = d.getUTCFullYear() +
               (d.getUTCMonth() + 1 + "").padStart(2, "0") +
               (d.getUTCDate() + "").padStart(2, "0");
  const time = (d.getUTCHours() + "").padStart(2, "0") +
               (d.getUTCMinutes() + "").padStart(2, "0") +
               (d.getUTCSeconds() + "").padStart(2, "0");

  // @ts-ignore
  abap.builtin.sy.get().datlo.set(date);
  // @ts-ignore
  abap.builtin.sy.get().datum.set(date);
  // @ts-ignore
  abap.builtin.sy.get().timlo.set(time);
  // @ts-ignore
  abap.builtin.sy.get().uzeit.set(time);

  if (options?.field) {
    options.field.set(time);
  }
  if (options?.stamp) {
    options.stamp.set(date + time);
  }
}
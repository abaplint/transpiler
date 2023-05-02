import {String, Table} from "../types/index.js";
import {ICharacter} from "../types/_character.js";

interface IReadReportOptions {
  state?: ICharacter,
  into?: Table,
}

export function readReport(name: ICharacter, options: IReadReportOptions) {

  if (options.into) {
    options.into.clear();
    options.into.append(new String().set("ReadReportTodo-" + name));
  }

// TODO

  // @ts-ignore
  abap.builtin.sy.get().subrc.set(0);
}
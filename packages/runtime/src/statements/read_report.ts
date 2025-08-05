import {String, Table} from "../types";
import {ICharacter} from "../types/_character";
import {ABAP} from "..";

declare const abap: ABAP;

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

  abap.builtin.sy.get().subrc.set(0);
}
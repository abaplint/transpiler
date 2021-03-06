import {ICharacter} from "../types/_character";
import {Table, String} from "../types";

export interface ISplitOptions {
  source: ICharacter | string,
  at: ICharacter | string,
  table?: Table,
  targets?: ICharacter[],
}

export function split(param: ISplitOptions) {
  const source = typeof param.source === "string" ? param.source : param.source.get();
  const at = typeof param.at === "string" ? param.at : param.at.get();

  const split = source.includes(at) ? source.split(at) : [];

  if (param.table) {
    param.table.clear();
    if (source.endsWith(at)) {
      split.pop();
    }
    for(const s of split) {
      param.table.append(new String().set(s));
    }
    if (source !== "" && split.length === 0) {
      param.table.append(new String().set(source));
    }
  }

  if (param.targets) {
    if (split.length === 0) {
      split.push(source);
    }
    for (let i = 0; i < param.targets.length; i++) {
      param.targets[i].clear();
      if (split[i]) {
        param.targets[i].set(split[i]);
      }
    }
  }

}
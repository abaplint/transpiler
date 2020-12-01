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

  if (param.table) {
    param.table.clear();
    for(const s of source.split(at)) {
      param.table.append(new String().set(s));
    }
  }

  if (param.targets) {
    const split = source.split(at);
    for (let i = 0; i < param.targets.length; i++) {
      param.targets[i].clear();
      if (split[i]) {
        param.targets[1].set(split[i]);
      }
    }
  }

}
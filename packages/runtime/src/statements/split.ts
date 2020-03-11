import {ICharacter} from "../types/_character";
import {Table, String} from "../types";

export function split(param: {source: ICharacter | string, at: ICharacter | string, target: Table}) {
  const source = typeof param.source === "string" ? param.source : param.source.get();
  const at = typeof param.at === "string" ? param.at : param.at.get();

  param.target.clear();
  for(const s of source.split(at)) {
    param.target.append(new String().set(s));
  }
}
import {ABAPObject, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function insertInternal(data: INumeric | ICharacter | Structure | ABAPObject, target: Table): void {
// todo, for now it just appends, this is not correct, but currently the table type is not known
  target.append(data);
}
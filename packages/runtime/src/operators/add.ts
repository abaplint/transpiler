import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function add(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  const ret = new Integer();
  // TODO, this is incomplete. Works with the correct datatypes, and a valid date as left.
  // However, depending on the target, we might not always want to interpret dates as dates... o_O
  if (left.constructor?.name === "Date") {
    ret.set(addDate(parse(left).toString().padStart(8,"0"), parse(right)));
  } else if (right.constructor?.name === "Date") {
    ret.set(addDate(parse(right).toString().padStart(8,"0"), parse(left)));
  } else {
    ret.set(parse(left) + parse(right));
  }
  return ret;
}

function addDate(left: string, right: number) {
  const year = parseInt(left.substr(0,4),10);
  const month = parseInt(left.substr(4,2),10) - 1;
  const day = parseInt(left.substr(6,2),10);
  const addToDate = new Date(year,month,day);

  addToDate.setDate(addToDate.getDate() + right);

  let newYear = 0;
  if (year < 100) {
    newYear = addToDate.getFullYear() - 1900;
  } else {
    newYear = addToDate.getFullYear();
  }
  let newDate = newYear.toString().padStart(4,"0");
  newDate += (addToDate.getMonth() + 1).toString().padStart(2,"0");
  newDate += addToDate.getDate().toString().padStart(2,"0");

  return new Integer().set(parseInt(newDate,10));
}
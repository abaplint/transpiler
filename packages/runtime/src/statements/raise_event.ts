import {ABAP} from "..";
import {ABAPEventReference} from "../abap_eventing";
import {ABAPObject} from "../types";

declare const abap: ABAP;

export async function raiseEvent(eventReference: ABAPEventReference, me: ABAPObject, parameters?: object) {
  const input = parameters || {}
  await abap.eventing.raiseEvent(eventReference, me, {...input, sender: me});
}
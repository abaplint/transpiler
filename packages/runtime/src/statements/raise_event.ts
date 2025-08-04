import {ABAP} from "..";
import {ABAPEventReference} from "../abap_eventing";
import {ABAPObject} from "../types";

declare const abap: ABAP;

export async function raiseEvent(eventReference: ABAPEventReference, me: ABAPObject, parameters?: object) {
  await abap.eventing.raiseEvent(eventReference, me, parameters);
}
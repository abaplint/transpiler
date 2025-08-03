import {ABAP} from "..";
import {ABAPEventReference} from "../abap_eventing";

declare const abap: ABAP;

export function raiseEvent(eventReference: ABAPEventReference, parameters?: object) {
  abap.eventing.raiseEvent(eventReference, parameters);
}
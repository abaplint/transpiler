export function raiseEvent(eventReference: any, parameters?: object) {
  // @ts-ignore
  eventReference.dispatchEvent(new abap.ABAPEvent('ABAP_EVENT', parameters));
}
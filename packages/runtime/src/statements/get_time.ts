export function getTime(): void {
  // @ts-ignore
  abap.builtin.sy.get().datlo.set(4);
  // @ts-ignore
  abap.builtin.sy.get().datum.set(4);
  // @ts-ignore
  abap.builtin.sy.get().timlo.set(4);
  // @ts-ignore
  abap.builtin.sy.get().uzeit.set(4);
}
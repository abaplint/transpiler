export function throwError(name: string) {
  // @ts-ignore
  if (abap.Classes[name] !== undefined) {
    // @ts-ignore
    throw new abap.Classes[name]();
  } else {
    throw new Error(`Global class ${name} not found`);
  }
}
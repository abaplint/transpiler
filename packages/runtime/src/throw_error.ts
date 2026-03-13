import {ABAP} from ".";

declare const abap: ABAP;

// todo: add possibility to pass parameters to the exception constructor
export function throwError(name: string, parameters?: object): never {
  if (abap.Classes[name] !== undefined) {
    throw new abap.Classes[name](parameters);
  } else {
    throw new Error(`Global class ${name} not found`);
  }
}
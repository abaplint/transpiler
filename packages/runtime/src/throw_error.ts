import {ABAP} from ".";

declare const abap: ABAP;

export async function throwErrorWithParameters(name: string, parameters?: object): Promise<never> {
  if (abap.Classes[name] !== undefined) {
    throw await new abap.Classes[name]().constructor_(parameters);
  } else {
    throw new Error(`Global class ${name} not found`);
  }
}

export function throwError(name: string): never {
  if (abap.Classes[name] !== undefined) {
    throw new abap.Classes[name]();
  } else {
    throw new Error(`Global class ${name} not found`);
  }
}
import {ABAP} from ".";

declare const abap: ABAP;

export async function throwErrorWithParameters(name: string, parameters?: object): Promise<never> {
  if (abap.Classes[name] !== undefined) {
    throw await new abap.Classes[name]().constructor_(parameters);
  } else {
    throw new Error(`Global class ${name} not found`);
  }
}

/** source position of the raising statement, read by CX_ROOT->GET_SOURCE_POSITION */
export type ExtraCX = {INTERNAL_FILENAME: string, INTERNAL_LINE: number};

export function throwError(name: string, extra?: ExtraCX): never {
  if (abap.Classes[name] !== undefined) {
    const error = new abap.Classes[name]();
    if (extra !== undefined) {
      error.EXTRA_CX = extra;
    }
    throw error;
  } else {
    throw new Error(`Global class ${name} not found`);
  }
}
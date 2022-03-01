export interface ICallFunctionOptions {
  name: string,
  destination: string,
  exporting?: any,
  importing?: any,
  tables?: any,
  changing?: any,
}

// note: this is only called if DESTNIATION is supplied
export function callFunction(_options: ICallFunctionOptions) {
  throw new Error("CALL FUNCTION DESTINATION, todo");
}
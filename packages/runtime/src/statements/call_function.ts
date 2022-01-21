export interface ICallFunctionOptions {
  name: string,
  destination: string,
  exporting?: any,
  importing?: any,
  tables?: any,
  changing?: any,
}

export function callFunction(_options: ICallFunctionOptions) {
  // note: this is only called if DESTNIATION is supplied

  // todo
  return;
}
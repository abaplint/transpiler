export interface INumeric {
  set(value: INumeric | number): void;
  get(): number;
}
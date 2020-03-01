export interface INumeric {
  set(value: INumeric | number): void;
  get(): number;
  clear(): void;
  eq(value: INumeric | number): boolean;
  ne(value: INumeric | number): boolean;
}
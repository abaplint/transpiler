export interface ICharacter {
  set(value: ICharacter | string): void;
  get(): string;
  clear(): void;
  eq(value: ICharacter | string): boolean;
  ne(value: ICharacter | string): boolean;
}
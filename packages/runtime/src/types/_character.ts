export interface ICharacter {
  set(value: ICharacter | string): void;
  get(): string;
  clear(): void;
}
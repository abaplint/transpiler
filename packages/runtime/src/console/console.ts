export interface Console {
  clear(): void;
  add(data: string): void;
  get(): string;
  getTrimmed(): string;
}
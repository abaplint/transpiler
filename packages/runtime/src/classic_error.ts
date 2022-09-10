export class ClassicError extends Error {
  public classic: string;
  public constructor(input: {classic: string}) {
    super();
    this.classic = input.classic;
  }
}
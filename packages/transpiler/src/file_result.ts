export class FileResult {
  private raw: string;

  public constructor() {
    this.raw = "";
  }

  public append(val: string) {
    this.raw += val;
  }

  public stripLastNewline(): void {
    if (this.raw.endsWith("\n")) {
      this.raw = this.raw.substring(0, this.raw.length - 1);
    }
  }

  public get(): string {
    return this.raw;
  }
}
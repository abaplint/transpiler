export class FileResult {
  private raw: string;
  private indentation: number;

  public constructor() {
    this.raw = "";
    this.indentation = 0;
  }

  public append(input: string) {
    const output: string[] = [];

    for (const l of input.split("\n")) {
      if (l.startsWith("}")) {
        this.indentation = this.indentation - 1;
      }
      if (this.indentation > 0) {
        output.push(" ".repeat(this.indentation * 2) + l);
      } else {
        output.push(l);
      }
      if (l.endsWith(" {")) {
        this.indentation = this.indentation + 1;
      }
    }

    this.raw += output.join("\n");
  }

  public stripLastNewline(): void {
    // note: this will not change the source map
    if (this.raw.endsWith("\n")) {
      this.raw = this.raw.substring(0, this.raw.length - 1);
    }
  }

  public get(): string {
    return this.raw;
  }
}
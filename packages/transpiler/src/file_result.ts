import * as sourceMap from "source-map";

// Performs automatic indentation
// Keeps track of source maps as generated code is added
export class FileResult {
  private raw: string;
  private indentation: number;
  private readonly map: sourceMap.SourceMapGenerator;

  public constructor() {
    this.raw = "";
    this.indentation = 0;
    this.map = new sourceMap.SourceMapGenerator();
  }

  public append(input: string) {
/*
    this.map.addMapping({
      generated: {
        line: 10,
        column: 35,
      },
      source: "foo.js",
      original: {
        line: 33,
        column: 2,
      },
      name: "christopher",
    });
*/

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

  public getCode(): string {
    return this.raw;
  }

  public getMap(generatedFilename: string): string {
    const json = this.map.toJSON();
    json.file = generatedFilename;
    json.sourceRoot = "";
    return JSON.stringify(json, null, 2);
  }
}
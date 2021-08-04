import * as sourceMap from "source-map";
import * as abaplint from "@abaplint/core";

/*
Source map:
line: The line number is 1-based.
column: The column number is 0-based.

abaplint:
line: The line number is 1-based.
column: The column number is 1-based.
*/

// Performs automatic indentation
// Keeps track of source maps as generated code is added
export class Chunk {
  private raw: string;
  private readonly map: sourceMap.SourceMapGenerator;

  public constructor(str?: string, pos?: abaplint.Position) {
    this.raw = "";
    this.map = new sourceMap.SourceMapGenerator();

    if (str) {
      this.appendString(str, pos);
    }
  }

  public appendChunk(input: Chunk) {
    this.raw += input.getCode();
  }

  public appendString(input: string, pos?: abaplint.Position) {
    if (pos) {
      const lines = this.raw.split("\n");
      const lastLine = lines[lines.length - 1];
      this.map.addMapping({
        source: "foo.js",
        generated: {
          line: lines.length + 1,
          column: lastLine.length,
        },
        original: {
          line: pos.getRow(),
          column: pos.getCol() - 1,
        },
      });
    }

/*
    const output: string[] = [];

    if (input === "\n") {
      const lines = this.raw.split("\n");
      const lastLine = lines[lines.length - 1];
      if (lastLine.startsWith("}")) {
        this.indentation = this.indentation - 1;
      } else if (lastLine.endsWith(" {")) {
        this.indentation = this.indentation + 1;
      }
      this.raw += "\n";
      return;
    }

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
    */
    this.raw += input;
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
import * as sourceMap from "source-map";
import * as abaplint from "@abaplint/core";

/*
source-map:
  line: The line number is 1-based.
  column: The column number is 0-based.

abaplint:
  line: The line number is 1-based.
  column: The column number is 1-based.
*/

// Keeps track of source maps as generated code is added
export class Chunk {
  private raw: string;
  public readonly map: sourceMap.Mapping[] = [];

  public constructor(str?: string, pos?: abaplint.Position) {
    this.raw = "";
    this.map = [];

    if (str) {
      this.appendString(str, pos);
    }
  }

  public appendChunk(append: Chunk) {
    const lines = this.raw.split("\n");
    const lineCount = lines.length;
    const lastLine = lines[lines.length - 1];
    append.map.forEach(m => {
      // original stays the same
      const add = m;
      if (add.generated.line === 1 && this.raw.endsWith("\n") === false) {
        add.generated.column += lastLine.length;
      } else {
        add.generated.line += lineCount - 1;
      }
      this.map.push(add);
    });

    this.raw += append.getCode();
  }

  public appendString(input: string, pos?: abaplint.Position) {
    if (pos) {
      const lines = this.raw.split("\n");
      const lastLine = lines[lines.length - 1];
      this.map.push({
        source: "zfoobar.prog.js",
        generated: {
          line: lines.length,
          column: lastLine.length,
        },
        original: {
          line: pos.getRow(),
          column: pos.getCol() - 1,
        },
      });
    }

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
    const generator = new sourceMap.SourceMapGenerator();

    this.map.forEach(m => {
      generator.addMapping(m);
    });

    const json = generator.toJSON();
    json.file = generatedFilename;
    json.sourceRoot = "";
    return JSON.stringify(json, null, 2);
  }
}
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
  public readonly mappings: sourceMap.Mapping[] = [];

  public constructor(str?: string) {
    this.raw = "";
    this.mappings = [];
    if (str) {
      this.appendString(str);
    }
  }

  public appendChunk(append: Chunk) {
    const lines = this.raw.split("\n");
    const lineCount = lines.length;
    const lastLine = lines[lines.length - 1];
    append.mappings.forEach(m => {
      // original stays the same, but adjust the generated positions
      const add = m;
      if (add.generated.line === 1 && this.raw.endsWith("\n") === false) {
        add.generated.column += lastLine.length;
      } else {
        add.generated.line += lineCount - 1;
      }
      this.mappings.push(add);
    });

    this.raw += append.getCode();
  }

  public append(input: string, pos: abaplint.Position | abaplint.INode, traversal: {getFilename(): string}) {
    if (pos) {
      const lines = this.raw.split("\n");
      const lastLine = lines[lines.length - 1];
      const originalLine = pos instanceof abaplint.Position ? pos.getRow() : pos.getFirstToken().getRow();
      const originalColumn = pos instanceof abaplint.Position ? pos.getCol() - 1 : pos.getFirstToken().getCol() - 1;
      this.mappings.push({
        source: traversal.getFilename(),
        generated: {
          line: lines.length,
          column: lastLine.length,
        },
        original: {
          line: originalLine,
          column: originalColumn,
        },
      });
    }

    this.raw += input;
    return this;
  }

  public appendString(input: string) {
    this.raw += input;
    return this;
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

  public toString(): string {
    throw "error, dont toString a Chunk";
  }

  public runIndentationLogic() {
    let i = 0;
    let line = 1;
    const output: string[] = [];

    for (const l of this.raw.split("\n")) {
      if (l.startsWith("}")) {
        i = i - 1;
      }
      if (i > 0) {
        output.push(" ".repeat(i * 2) + l);
      } else {
        output.push(l);
      }

// fix maps
      for (const m of this.mappings) {
        if (m.generated.line === line) {
          m.generated.column += i * 2;
        }
      }

      if (l.endsWith(" {")) {
        i = i + 1;
      }

      line++;
    }

    this.raw = output.join("\n");
    return this;
  }

  public getMap(generatedFilename: string): string {
    const generator = new sourceMap.SourceMapGenerator();
    this.mappings.forEach(m => generator.addMapping(m));

    const json = generator.toJSON();
    json.file = generatedFilename;
    json.sourceRoot = "";
    return JSON.stringify(json, null, 2);
  }
}
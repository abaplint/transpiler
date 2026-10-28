import * as sourceMap from "source-map";
import * as abaplint from "@abaplint/core";

type SourceMapTraversal = {
  getFilename(): string;
  isSourceMapEnabled?(): boolean;
};

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
  // tracked incrementally so appends never re-scan the whole buffer
  private lineCount: number;
  private lastLineLength: number;
  public mappings: sourceMap.Mapping[] = [];

  public constructor(str?: string) {
    this.raw = "";
    this.lineCount = 1;
    this.lastLineLength = 0;
    this.mappings = [];
    if (str) {
      this.appendString(str);
    }
  }

  public join(chunks: Chunk[], str = ", "): Chunk {
    for (let i = 0; i < chunks.length; i++) {
      this.appendChunk(chunks[i]);
      if (i !== chunks.length - 1) {
        this.appendString(str);
      }
    }
    return this;
  }

  public appendChunk(append: Chunk): Chunk {
    if (append.getCode() === "") {
      return this;
    }

    for (const m of append.mappings) {
      // deep copy so the appended chunk's mappings are never mutated,
      // otherwise appending the same chunk twice double-shifts its positions
      const add: sourceMap.Mapping = {
        source: m.source,
        name: m.name,
        generated: {line: m.generated.line, column: m.generated.column},
        original: {line: m.original.line, column: m.original.column},
      };
      // original stays the same, but adjust the generated positions:
      // the appended content begins at the end of the current buffer, so its
      // first line continues the current last line, and every line moves down
      if (add.generated.line === 1) {
        add.generated.column += this.lastLineLength;
      }
      add.generated.line += this.lineCount - 1;
      this.mappings.push(add);
    }

    return this.appendString(append.getCode());
  }

  private originalPosition(pos: abaplint.Position | abaplint.INode | abaplint.Token): {line: number, column: number} {
    if (pos instanceof abaplint.Position || pos instanceof abaplint.Token) {
      return {line: pos.getRow(), column: pos.getCol() - 1};
    } else {
      return {line: pos.getFirstToken().getRow(), column: pos.getFirstToken().getCol() - 1};
    }
  }

  public append(input: string, pos: abaplint.Position | abaplint.INode | abaplint.Token, traversal: SourceMapTraversal): Chunk {
    if (input === "") {
      return this;
    }

    if (pos && input !== "\n" && traversal.isSourceMapEnabled?.() !== false) {
      this.mappings.push({
        source: traversal.getFilename(),
        generated: {
          line: this.lineCount,
          column: this.lastLineLength,
        },
        original: this.originalPosition(pos),
      });
    }

    return this.appendString(input);
  }

  /**
   * Baseline fallback so statements whose transpiler emitted no mappings still
   * resolve to their ABAP source. Adds a single mapping from the start of this
   * chunk (generated line 1, column 0) to `pos`. No-op if the chunk is empty or
   * already carries mappings, so it never overrides finer-grained mappings.
   */
  public ensureStartMapping(pos: abaplint.Position | abaplint.INode | abaplint.Token, traversal: SourceMapTraversal): Chunk {
    if (this.raw === "" || this.mappings.length > 0 || traversal.isSourceMapEnabled?.() === false) {
      return this;
    }
    this.mappings.push({
      source: traversal.getFilename(),
      generated: {line: 1, column: 0},
      original: this.originalPosition(pos),
    });
    return this;
  }

  public appendString(input: string) {
    this.raw += input;
    const lastNewline = input.lastIndexOf("\n");
    if (lastNewline < 0) {
      this.lastLineLength += input.length;
    } else {
      this.lineCount += input.split("\n").length - 1;
      this.lastLineLength = input.length - lastNewline - 1;
    }
    return this;
  }

  /** Replace generated code while keeping source-map positions after every
   * occurrence aligned, including replacements that add new lines. */
  public replaceAll(search: string, replacement: string): Chunk {
    if (search === "") {
      throw new Error("Chunk.replaceAll, search string must not be empty");
    }
    const positions: number[] = [];
    let cursor = 0;
    while (true) {
      const index = this.raw.indexOf(search, cursor);
      if (index < 0) {
        break;
      }
      positions.push(index);
      cursor = index + search.length;
    }

    const replacementLines = replacement.split("\n");
    const lineDelta = replacementLines.length - 1;
    const replacementLastLineLength = replacementLines[replacementLines.length - 1].length;
    for (const index of positions.reverse()) {
      const before = this.raw.substring(0, index);
      const startLine = before.split("\n").length;
      const startColumn = before.length - before.lastIndexOf("\n") - 1;
      const endColumn = startColumn + search.length;

      for (const mapping of this.mappings) {
        if (mapping.generated.line > startLine) {
          mapping.generated.line += lineDelta;
        } else if (mapping.generated.line === startLine && mapping.generated.column >= endColumn) {
          if (lineDelta === 0) {
            mapping.generated.column += replacement.length - search.length;
          } else {
            mapping.generated.line += lineDelta;
            mapping.generated.column = replacementLastLineLength + mapping.generated.column - endColumn;
          }
        }
      }

      this.raw = this.raw.substring(0, index) + replacement + this.raw.substring(index + search.length);
    }

    const lastNewline = this.raw.lastIndexOf("\n");
    this.lineCount = this.raw.split("\n").length;
    this.lastLineLength = this.raw.length - lastNewline - 1;
    return this;
  }

  public stripLastNewline(): void {
    // note: this will not change the source map
    if (this.raw.endsWith("\n")) {
      this.raw = this.raw.substring(0, this.raw.length - 1);
      this.lineCount--;
      this.lastLineLength = this.raw.length - this.raw.lastIndexOf("\n") - 1;
    }
  }

  public getCode(): string {
    return this.raw;
  }

  public toString(): string {
    throw "error, dont toString a Chunk";
  }

  public runIndentationLogic(ignoreSourceMap = false) {
    let i = 0;
    let line = 1;
    const output: string[] = [];

    if (ignoreSourceMap === true) {
      this.mappings = [];
    }
    const indentationByLine: number[] | undefined = this.mappings.length > 0 ? [] : undefined;

    for (const l of this.raw.split("\n")) {
      // trimStart, a pre-indented closing brace must still close its level
      if (l.trimStart().startsWith("}")) {
        i = i - 1;
      }
      // clamp so unbalanced braces never produce a negative indent/shift
      const indent = i > 0 ? i * 2 : 0;
      if (indentationByLine !== undefined) {
        indentationByLine[line] = indent;
      }
      if (indent > 0) {
        output.push(" ".repeat(indent) + l);
      } else {
        output.push(l);
      }

      if (l.endsWith(" {")) {
        i = i + 1;
      }

      line++;
    }

    // Shift every mapping once. The previous line-major implementation
    // inspected every mapping for every generated line (O(lines * mappings)).
    for (const m of this.mappings) {
      m.generated.column += indentationByLine?.[m.generated.line] || 0;
    }

    this.raw = output.join("\n");
    // line structure is unchanged, but the last line may have been indented
    this.lastLineLength = output[output.length - 1].length;
    return this;
  }

  /**
   * @param generatedFilename name written to the "file" field of the map
   * @param options.generatedLineOffset number of lines prepended to the generated
   *   output after this chunk was built (e.g. a runtime import line); every mapping
   *   is shifted down by this amount so the map stays aligned with the file on disk
   * @param options.sourcePaths maps a mapping "source" (the bare abap filename) to
   *   the path that should appear in the map, avoiding fragile post-hoc string edits
   */
  public getMap(generatedFilename: string, options?: {
    generatedLineOffset?: number,
    sourcePaths?: {[filename: string]: string},
    sourceContents?: {[filename: string]: string},
  }): string {
    const offset = options?.generatedLineOffset ?? 0;
    const sourcePaths = options?.sourcePaths ?? {};

    const sourceMapGenerator = new sourceMap.SourceMapGenerator();
    this.mappings.forEach(m => sourceMapGenerator.addMapping({
      source: sourcePaths[m.source] ?? m.source,
      name: m.name,
      original: m.original,
      generated: {line: m.generated.line + offset, column: m.generated.column},
    }));

    for (const source of new Set(this.mappings.map(m => m.source))) {
      const contents = options?.sourceContents?.[source];
      if (contents !== undefined) {
        sourceMapGenerator.setSourceContent(sourcePaths[source] ?? source, contents);
      }
    }
    const json = sourceMapGenerator.toJSON();
    json.file = generatedFilename;
    json.sourceRoot = "";
    return JSON.stringify(json, null, 2);
  }
}

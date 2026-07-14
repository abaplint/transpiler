import {Transpiler} from "../src";
import {IFile, ITranspilerOptions} from "../src/types";
import {UniqueIdentifier} from "../src/unique_identifier";
import * as abaplint from "@abaplint/core";
import * as sourceMap from "source-map";

async function runFiles(files: IFile[], options?: ITranspilerOptions) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
  const res = await new Transpiler(options).run(reg);
  return res;
}

export async function runSingle(abap: string, options?: ITranspilerOptions): Promise<string | undefined> {
  UniqueIdentifier.reset();
  const res = await runFiles([{filename: "zfoobar.prog.abap", contents: abap}], options);
  return res.objects[0]?.chunk.getCode();
}

export async function runSingleMapped(abap: string, options?: ITranspilerOptions, filename = "zfoobar.prog.abap") {
  UniqueIdentifier.reset();
  const res = await runFiles([{filename, contents: abap}], options);
  const obj = res.objects[0];
  if (obj === undefined) {
    return undefined;
  }
  return {
    js: obj.chunk.getCode(),
    map: obj.chunk.getMap("zfoobar.prog.mjs"),
  };
}

export interface IMapValidation {
  /** number of non-empty generated lines */
  codeLines: number;
  /** how many of those resolve to an original position */
  mappedLines: number;
  /** generated lines (1-based) with code but no mapping */
  unmappedLines: number[];
}

/**
 * Validate a generated file against its source map: returns coverage stats and
 * asserts that every mapping's original position falls inside the given ABAP
 * source (line within range, column non-negative). Throws on an out-of-range
 * mapping, which would indicate a broken position calculation.
 */
export async function validateSourceMap(abap: string, js: string, map: string): Promise<IMapValidation> {
  const abapLines = abap.split("\n");
  const consumer = await new sourceMap.SourceMapConsumer(JSON.parse(map));

  // every mapping must point somewhere real in the source
  consumer.eachMapping(m => {
    if (m.originalLine === null) {
      return;
    }
    if (m.originalLine < 1 || m.originalLine > abapLines.length) {
      throw new Error(`source map: original line ${m.originalLine} out of range (1..${abapLines.length})`);
    }
    if (m.originalColumn < 0) {
      throw new Error(`source map: negative original column ${m.originalColumn} on generated line ${m.generatedLine}`);
    }
  });

  const jsLines = js.split("\n");
  const unmappedLines: number[] = [];
  let codeLines = 0;
  let mappedLines = 0;
  jsLines.forEach((l, i) => {
    if (l.trim() === "") {
      return;
    }
    codeLines++;
    const col = l.length - l.trimStart().length;
    const orig = consumer.originalPositionFor({line: i + 1, column: col});
    if (orig.line === null) {
      unmappedLines.push(i + 1);
    } else {
      mappedLines++;
    }
  });

  return {codeLines, mappedLines, unmappedLines};
}
import * as abaplint from "@abaplint/core";

/** the files of the object, for programs also the files of the include programs expanded into it */
export function filesWithProgramIncludes(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): readonly abaplint.ABAPFile[] {
  const files = obj.getSequencedFiles();
  if (!(obj instanceof abaplint.Objects.Program) || obj.isInclude() === true) {
    return files;
  }
  const ret = [...files];
  const visited = new Set<string>();
  for (const file of files) {
    ret.push(...findProgramIncludes(file, reg, visited));
  }
  return ret;
}

/** include programs referenced by INCLUDE statements in the file, recursively, each returned once */
export function findProgramIncludes(file: abaplint.ABAPFile, reg: abaplint.IRegistry, visited = new Set<string>()): abaplint.ABAPFile[] {
  const ret: abaplint.ABAPFile[] = [];
  for (const statement of file.getStructure()?.findAllStatements(abaplint.Statements.Include) || []) {
    const name = statement.findDirectExpression(abaplint.Expressions.IncludeName)?.concatTokens().toUpperCase();
    if (name === undefined || visited.has(name)) {
      continue;
    }
    visited.add(name);
    const include = (reg.getObject("PROG", name) as abaplint.Objects.Program | undefined)?.getMainABAPFile();
    if (include !== undefined) {
      ret.push(include, ...findProgramIncludes(include, reg, visited));
    }
  }
  return ret;
}

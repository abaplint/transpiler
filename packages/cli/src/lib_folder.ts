import * as path from "path";

// A lib "folder" is relative to the current working directory. It used to be
// concatenated onto cwd, so it only worked when it started with a separator;
// "deps" silently became "/path/to/cwddeps" and the lib was cloned instead.
// join() keeps the leading separator working, so existing configuration is
// unaffected, and accepts "deps" and "./deps" as well.
export function resolveLibFolder(folder: string | undefined, cwd: string): string | undefined {
  if (folder === undefined || folder === "") {
    return undefined;
  }
  return path.join(cwd, folder);
}

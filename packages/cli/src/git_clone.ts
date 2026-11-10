import * as childProcess from "child_process";

/** a full or abbreviated commit SHA, as opposed to a branch or tag name */
export function isCommitSha(ref: string): boolean {
  return /^[0-9a-f]{7,40}$/i.test(ref);
}

/** shallow clone of the default branch, or of the branch or tag `ref` */
export function buildGitCloneArguments(url: string, ref?: string): string[] {
  const branch = ref === undefined || ref === "" ? [] : ["--branch", ref];
  return ["clone", "--quiet", "--depth", "1", ...branch, "--", url, "."];
}

function git(args: string[], cwd: string, stderr: "inherit" | "pipe" = "inherit"): void {
  childProcess.execFileSync("git", args, {cwd, stdio: ["ignore", "inherit", stderr]});
}

/** Clones `url` into the empty directory `dir` and returns the commit checked out.
 *
 * Without `ref` this is a shallow clone of the default branch. A branch or tag
 * is cloned shallow with --branch. A commit SHA is fetched shallow by name,
 * which GitHub and every server with uploadpack.allowReachableSHA1InWant
 * serve; when the server refuses that, or the SHA is abbreviated, the full
 * history is fetched and the commit checked out from it. */
export function cloneRepository(url: string, dir: string, ref?: string): string {
  if (ref === undefined || ref === "") {
    git(buildGitCloneArguments(url), dir);
  } else if (ref.startsWith("-")) {
    throw new Error("Invalid lib ref: " + JSON.stringify(ref));
  } else if (isCommitSha(ref)) {
    git(["init", "--quiet"], dir);
    git(["remote", "add", "origin", "--", url], dir);
    try {
      git(["fetch", "--quiet", "--depth", "1", "origin", ref], dir, "pipe");
      git(["checkout", "--quiet", "--detach", "FETCH_HEAD"], dir);
    } catch {
      console.log("\tshallow fetch of " + ref + " not served, fetching the full history");
      git(["fetch", "--quiet", "--tags", "origin"], dir);
      git(["checkout", "--quiet", "--detach", ref], dir);
    }
  } else {
    git(buildGitCloneArguments(url, ref), dir);
  }
  return resolveHead(dir)!;
}

/** the commit checked out in `dir`, undefined when it is not a git checkout */
export function resolveHead(dir: string): string | undefined {
  try {
    return childProcess.execFileSync("git", ["rev-parse", "HEAD"], {cwd: dir, encoding: "utf8", stdio: ["ignore", "pipe", "ignore"]}).trim();
  } catch {
    return undefined;
  }
}

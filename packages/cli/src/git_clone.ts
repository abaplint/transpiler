export function buildGitCloneArguments(url: string): string[] {
  return ["clone", "--quiet", "--depth", "1", "--", url, "."];
}

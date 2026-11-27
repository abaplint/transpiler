import {expect} from "chai";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as childProcess from "child_process";
import {buildGitCloneArguments, cloneRepository, isCommitSha, resolveHead} from "../packages/cli/src/git_clone";

describe("git clone", () => {
  it("keeps the URL separate from the command", () => {
    const url = "https://example.com/repository.git;echo injected";

    expect(buildGitCloneArguments(url)).to.deep.equal([
      "clone",
      "--quiet",
      "--depth",
      "1",
      "--",
      url,
      ".",
    ]);
  });

  it("protects a URL that starts with a dash from option injection", () => {
    const url = "--upload-pack=echo injected";

    expect(buildGitCloneArguments(url)).to.deep.equal([
      "clone",
      "--quiet",
      "--depth",
      "1",
      "--",
      url,
      ".",
    ]);
  });

  it("clones a branch or tag shallow with --branch", () => {
    const url = "https://example.com/repository.git";

    expect(buildGitCloneArguments(url, "v1.2.0")).to.deep.equal([
      "clone",
      "--quiet",
      "--depth",
      "1",
      "--branch",
      "v1.2.0",
      "--",
      url,
      ".",
    ]);
    expect(buildGitCloneArguments(url, "")).to.deep.equal(buildGitCloneArguments(url));
  });

  it("tells a commit SHA from a branch or tag", () => {
    expect(isCommitSha("239b817e6fd05e62854fed25663f002c60dda07c")).to.equal(true);
    expect(isCommitSha("239b817")).to.equal(true);
    expect(isCommitSha("main")).to.equal(false);
    expect(isCommitSha("v1.2.0")).to.equal(false);
    expect(isCommitSha("deadbeef-branch")).to.equal(false);
    expect(isCommitSha("abc")).to.equal(false);
  });
});

describe.skip("git clone, checking out a ref", () => {
  let root: string;
  let url: string;
  let first: string;
  let second: string;
  const dirs: string[] = [];

  function git(args: string[], cwd: string): string {
    return childProcess.execFileSync("git", ["-c", "user.email=test@example.com", "-c", "user.name=test", ...args],
                                     {cwd, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"]}).trim();
  }

  function target(): string {
    const dir = fs.mkdtempSync(path.join(root, "clone-"));
    dirs.push(dir);
    return dir;
  }

  before(() => {
    // a local repository with two commits, a tag and a branch at the first one
    root = fs.mkdtempSync(path.join(os.tmpdir(), "abap_transpile-git-"));
    const source = path.join(root, "source");
    fs.mkdirSync(source);
    git(["init", "--quiet", "--initial-branch=main"], source);
    fs.writeFileSync(path.join(source, "zfirst.prog.abap"), "WRITE 'first'.");
    git(["add", "."], source);
    git(["commit", "--quiet", "-m", "first"], source);
    first = git(["rev-parse", "HEAD"], source);
    git(["tag", "v1"], source);
    git(["branch", "feature"], source);
    fs.writeFileSync(path.join(source, "zsecond.prog.abap"), "WRITE 'second'.");
    git(["add", "."], source);
    git(["commit", "--quiet", "-m", "second"], source);
    second = git(["rev-parse", "HEAD"], source);
    const bare = path.join(root, "bare.git");
    git(["clone", "--quiet", "--bare", source, bare], root);
    url = "file://" + bare.replace(/\\/g, "/");
  });

  after(() => {
    fs.rmSync(root, {recursive: true, force: true});
  });

  it("without a ref clones the default branch", () => {
    const dir = target();
    expect(cloneRepository(url, dir)).to.equal(second);
    expect(fs.existsSync(path.join(dir, "zsecond.prog.abap"))).to.equal(true);
  });

  it("checks out a full commit SHA", () => {
    const dir = target();
    expect(cloneRepository(url, dir, first)).to.equal(first);
    expect(resolveHead(dir)).to.equal(first);
    expect(fs.existsSync(path.join(dir, "zsecond.prog.abap"))).to.equal(false);
  });

  it("checks out an abbreviated commit SHA via the full fetch", () => {
    const dir = target();
    expect(cloneRepository(url, dir, first.substring(0, 7))).to.equal(first);
  });

  it("checks out a tag", () => {
    const dir = target();
    expect(cloneRepository(url, dir, "v1")).to.equal(first);
  });

  it("checks out a branch", () => {
    const dir = target();
    expect(cloneRepository(url, dir, "feature")).to.equal(first);
  });

  it("rejects a ref that starts with a dash", () => {
    expect(() => cloneRepository(url, target(), "--upload-pack=echo injected")).to.throw("Invalid lib ref");
  });

  it("fails on an unknown ref", () => {
    expect(() => cloneRepository(url, target(), "no-such-branch")).to.throw();
  });

  it("resolveHead is undefined outside a git checkout", () => {
    expect(resolveHead(os.tmpdir())).to.equal(undefined);
  });
});

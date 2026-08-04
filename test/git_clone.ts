import {expect} from "chai";
import {buildGitCloneArguments} from "../packages/cli/src/git_clone";

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
});

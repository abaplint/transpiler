import {expect} from "chai";
import {join} from "path";
import {resolveLibFolder} from "../packages/cli/src/lib_folder";

describe("resolveLibFolder", () => {
  it("no folder configured", () => {
    expect(resolveLibFolder(undefined, "/work")).to.equal(undefined);
    expect(resolveLibFolder("", "/work")).to.equal(undefined);
  });

  it("a leading separator still means below the current directory", () => {
    expect(resolveLibFolder("/deps", "/work")).to.equal(join("/work", "deps"));
  });

  it("a relative folder is resolved from the current directory", () => {
    expect(resolveLibFolder("deps", "/work")).to.equal(join("/work", "deps"));
    expect(resolveLibFolder("./deps", "/work")).to.equal(join("/work", "deps"));
  });

  it("the separator is neither doubled nor dropped", () => {
    expect(resolveLibFolder("deps/open-abap-core", "/work")).to.equal(join("/work", "deps", "open-abap-core"));
    expect(resolveLibFolder("/deps/open-abap-core", "/work/")).to.equal(join("/work", "deps", "open-abap-core"));
  });
});

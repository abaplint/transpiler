import {expect} from "chai";
import {existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, statSync, symlinkSync, utimesSync, writeFileSync} from "fs";
import {createRequire} from "node:module";
import {tmpdir} from "os";
import * as path from "path";
import {IOutputArtifact, writeOutput} from "../packages/cli/src/output_writer";

describe("Incremental output writer", () => {
  let folder: string;
  let output: string;
  const artifact = (name: string, contents: string): IOutputArtifact => ({path: name, contents});

  beforeEach(() => {
    folder = mkdtempSync(path.join(tmpdir(), "abaplint-output-writer-"));
    output = path.join(folder, "output");
  });

  afterEach(() => {
    rmSync(folder, {recursive: true, force: true});
  });

  it("writes new files, skips identical files, and preserves modification times", async () => {
    const files = [artifact("project/app.mjs", "export const value = 1;")];
    const first = await writeOutput(output, files, true);
    expect(first.created).to.equal(1);
    const target = path.join(output, "project/app.mjs");
    const manifest = path.join(output, ".abap-transpile-manifest.json");
    expect(JSON.parse(readFileSync(manifest, "utf8")).files).to.deep.equal(["project/app.mjs"]);
    const knownTime = new Date("2020-01-02T03:04:06.000Z");
    utimesSync(target, knownTime, knownTime);
    utimesSync(manifest, knownTime, knownTime);

    const second = await writeOutput(output, files, true);
    expect(second).to.deep.equal({created: 0, updated: 0, unchanged: 1, deleted: 0});
    expect(statSync(target).mtimeMs).to.equal(knownTime.getTime());
    expect(statSync(manifest).mtimeMs).to.equal(knownTime.getTime());
  });

  it("rewrites same-size edits and creates missing files", async () => {
    await writeOutput(output, [artifact("app.mjs", "original")], true);
    writeFileSync(path.join(output, "app.mjs"), "modified");
    rmSync(path.join(output, "missing.mjs"), {force: true});

    const result = await writeOutput(output, [
      artifact("app.mjs", "updated!"),
      artifact("missing.mjs", "created"),
    ], true);
    expect(result.updated).to.equal(1);
    expect(result.created).to.equal(1);
    expect(readFileSync(path.join(output, "app.mjs"), "utf8")).to.equal("updated!");
    expect(readFileSync(path.join(output, "missing.mjs"), "utf8")).to.equal("created");
  });

  it("skips content reads for different-size files and preserves unrelated output", async () => {
    mkdirSync(path.join(output, "obsolete-lib"), {recursive: true});
    writeFileSync(path.join(output, "obsolete-lib/unowned.txt"), "keep");
    await writeOutput(output, [
      artifact("obsolete-lib/owned.mjs", "old"),
      artifact("stable.mjs", "same"),
      artifact("size.mjs", "x"),
    ], true);
    writeFileSync(path.join(output, "custom.mjs"), "keep");

    const nativeFs = createRequire(__filename)("node:fs/promises") as typeof import("node:fs/promises");
    const originalReadFile = nativeFs.readFile;
    const reads: string[] = [];
    nativeFs.readFile = (async (...args: Parameters<typeof originalReadFile>) => {
      reads.push(String(args[0]));
      return originalReadFile(...args);
    }) as typeof originalReadFile;
    try {
      const result = await writeOutput(output, [
        artifact("stable.mjs", "same"),
        artifact("size.mjs", "longer"),
        artifact("missing.mjs", "new"),
      ], true);
      expect(result.deleted).to.equal(1);
    } finally {
      nativeFs.readFile = originalReadFile;
    }
    expect(reads.map(filename => path.basename(filename)).sort())
      .to.deep.equal([".abap-transpile-manifest.json", "stable.mjs"]);
    expect(readFileSync(path.join(output, "size.mjs"), "utf8")).to.equal("longer");
    expect(existsSync(path.join(output, "obsolete-lib/owned.mjs"))).to.equal(false);
    expect(readFileSync(path.join(output, "obsolete-lib/unowned.txt"), "utf8")).to.equal("keep");
    expect(readFileSync(path.join(output, "custom.mjs"), "utf8")).to.equal("keep");
  });

  it("does not infer stale-file ownership when the manifest is missing", async () => {
    mkdirSync(output, {recursive: true});
    writeFileSync(path.join(output, "old.mjs"), "legacy output");
    const result = await writeOutput(output, [artifact("current.mjs", "current")], true);
    expect(result.deleted).to.equal(0);
    expect(existsSync(path.join(output, "old.mjs"))).to.equal(true);
    expect(readFileSync(path.join(output, "current.mjs"), "utf8")).to.equal("current");
  });

  it("preserves binary data byte for byte", async () => {
    const bytes = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x00, 0x80, 0xff]);
    const file = artifact("image.w3mi.data.png", bytes.toString("latin1"));
    await writeOutput(output, [file], true);
    const result = await writeOutput(output, [file], true);
    expect(result.unchanged).to.equal(1);
    expect(readFileSync(path.join(output, file.path)).equals(bytes)).to.equal(true);
  });

  it("rejects invalid manifests before writing artifacts", async () => {
    mkdirSync(output, {recursive: true});
    writeFileSync(path.join(output, ".abap-transpile-manifest.json"), "{");
    let failed = false;
    try {
      await writeOutput(output, [artifact("new.mjs", "new")], true);
    } catch {
      failed = true;
    }
    expect(failed).to.equal(true);
    expect(existsSync(path.join(output, "new.mjs"))).to.equal(false);
  });

  it("rejects escaping destinations and symlinked output parents", async () => {
    let escaped = false;
    try {
      await writeOutput(output, [artifact("../escape.mjs", "outside")], true);
    } catch {
      escaped = true;
    }
    expect(escaped).to.equal(true);
    expect(existsSync(path.join(folder, "escape.mjs"))).to.equal(false);

    const outside = path.join(folder, "outside");
    mkdirSync(outside);
    mkdirSync(output);
    symlinkSync(outside, path.join(output, "linked"), process.platform === "win32" ? "junction" : "dir");
    let rejectedLink = false;
    try {
      await writeOutput(output, [artifact("linked/output.mjs", "outside")], true);
    } catch {
      rejectedLink = true;
    }
    expect(rejectedLink).to.equal(true);
    expect(existsSync(path.join(outside, "output.mjs"))).to.equal(false);
  });

  it("rejects case-insensitive, file-directory, and manifest path collisions before writing", async () => {
    const invalidSets = [
      [artifact("App.mjs", "one"), artifact("app.mjs", "two")],
      [artifact("parent", "one"), artifact("parent/child.mjs", "two")],
      [artifact(".abap-transpile-manifest.json/child", "reserved")],
    ];
    for (const incremental of [false, true]) {
      for (const files of invalidSets) {
        let rejected = false;
        try {
          await writeOutput(output, files, incremental);
        } catch {
          rejected = true;
        }
        expect(rejected).to.equal(true);
        expect(existsSync(output)).to.equal(false);
      }
    }
  });

  it("retains union ownership through a failed write and retries stale-file cleanup", async () => {
    await writeOutput(output, [artifact("keep.mjs", "keep"), artifact("old.mjs", "old")], true);
    const nativeFs = createRequire(__filename)("node:fs/promises") as typeof import("node:fs/promises");
    const originalWriteFile = nativeFs.writeFile;
    nativeFs.writeFile = (async (...args: Parameters<typeof originalWriteFile>) => {
      if (String(args[0]).endsWith("new.mjs")) {
        throw new Error("injected write failure");
      }
      return originalWriteFile(...args);
    }) as typeof originalWriteFile;
    try {
      let failed = false;
      try {
        await writeOutput(output, [artifact("keep.mjs", "keep"), artifact("new.mjs", "new")], true);
      } catch {
        failed = true;
      }
      expect(failed).to.equal(true);
    } finally {
      nativeFs.writeFile = originalWriteFile;
    }

    const ownership = JSON.parse(readFileSync(path.join(output, ".abap-transpile-manifest.json"), "utf8"));
    expect(ownership.files).to.include("old.mjs").and.include("new.mjs");
    expect(existsSync(path.join(output, "old.mjs"))).to.equal(true);
    expect(existsSync(path.join(output, "new.mjs"))).to.equal(false);

    const retry = await writeOutput(output, [artifact("keep.mjs", "keep"), artifact("new.mjs", "new")], true);
    expect(retry.created).to.equal(1);
    expect(retry.deleted).to.equal(1);
    expect(existsSync(path.join(output, "old.mjs"))).to.equal(false);
    expect(readFileSync(path.join(output, "new.mjs"), "utf8")).to.equal("new");
  });

  it("keeps legacy mode unconditional and does not prune stale files", async () => {
    await writeOutput(output, [artifact("app.mjs", "original"), artifact("old.mjs", "stale")], false);
    const target = path.join(output, "app.mjs");
    const knownTime = new Date("2020-01-02T03:04:06.000Z");
    utimesSync(target, knownTime, knownTime);

    const result = await writeOutput(output, [artifact("app.mjs", "original")], false);
    expect(result.updated).to.equal(1);
    expect(statSync(target).mtimeMs).to.be.greaterThan(knownTime.getTime());
    expect(existsSync(path.join(output, "old.mjs"))).to.equal(true);
    expect(existsSync(path.join(output, ".abap-transpile-manifest.json"))).to.equal(false);
  });
});

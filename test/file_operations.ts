import {expect} from "chai";
import {mkdtempSync, readFileSync, rmSync, writeFileSync} from "fs";
import {tmpdir} from "os";
import {join} from "path";
import {FileOperations} from "../packages/cli/src/file_operations";

// A PNG header plus bytes on both sides of 0x7F. Read as UTF-8 every byte
// above 0x7F fails to decode and comes back as the replacement character, so
// the file grows and is no longer a PNG; nothing reports an error, because a
// corrupted PNG is a perfectly valid file.
const BYTES = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x7f, 0x80, 0xff, 0xfe, 0x41]);

describe("FileOperations, a binary file survives the copy", () => {
  let folder = "";

  beforeEach(() => {
    folder = mkdtempSync(join(tmpdir(), "abaplint-binary-"));
  });

  afterEach(() => {
    rmSync(folder, {recursive: true, force: true});
  });

  it("a w3mi data file is read and written byte for byte", async () => {
    const source = join(folder, "zfoo%2epng.w3mi.data.png");
    writeFileSync(source, BYTES);

    const read = await FileOperations.readAllFiles([source], folder);
    expect(read.length).to.equal(1);

    const target = join(folder, "out.w3mi.data.png");
    await FileOperations.writeFiles([{path: target, contents: read[0].contents}]);

    expect(readFileSync(target).equals(BYTES)).to.equal(true);
  });

  it("an smim data file too", async () => {
    const source = join(folder, "zbar.smim.data.woff");
    writeFileSync(source, BYTES);
    const read = await FileOperations.readAllFiles([source], folder);
    const target = join(folder, "out.smim.data.woff");
    await FileOperations.writeFiles([{path: target, contents: read[0].contents}]);
    expect(readFileSync(target).equals(BYTES)).to.equal(true);
  });

  it("an ABAP file is still text", async () => {
    const source = join(folder, "zfoo.prog.abap");
    writeFileSync(source, "WRITE 'æøå'.", "utf8");
    const read = await FileOperations.readAllFiles([source], folder);
    expect(read[0].contents).to.equal("WRITE 'æøå'.");
  });

  it("only the data file is binary, not the XML beside it", () => {
    expect(FileOperations.isBinaryFilename("zfoo.w3mi.data.png")).to.equal(true);
    expect(FileOperations.isBinaryFilename("zfoo.smim.data.woff")).to.equal(true);
    expect(FileOperations.isBinaryFilename("zfoo.w3mi.xml")).to.equal(false);
    expect(FileOperations.isBinaryFilename("zfoo.prog.abap")).to.equal(false);
  });
});

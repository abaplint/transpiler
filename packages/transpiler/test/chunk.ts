import {expect} from "chai";
import * as sourceMap from "source-map";
import {Position} from "@abaplint/core";
import {Chunk} from "../src/chunk";

class Dummy {
  public getFilename() {
    return "dummy";
  }
}

describe("Chunk", () => {

  it("append lines", async () => {
    const chunk1 = new Chunk().append("START", new Position(1, 1), new Dummy());
    chunk1.appendString("\n");
    const chunk2 = new Chunk().append("END", new Position(2, 1), new Dummy());
    chunk1.appendChunk(chunk2);

    expect(chunk1.getCode()).to.equal("START\nEND");
    expect(chunk1.mappings.length).to.equal(2);

    expect(chunk1.mappings[0].generated.line).to.equal(1);
    expect(chunk1.mappings[0].original.line).to.equal(1);

    expect(chunk1.mappings[1].generated.line).to.equal(2);
    expect(chunk1.mappings[1].original.line).to.equal(2);
  });

  it("append on same line", async () => {
    const chunk1 = new Chunk().append("123", new Position(1, 1), new Dummy());
    const chunk2 = new Chunk().append("456", new Position(1, 4), new Dummy());
    chunk1.appendChunk(chunk2);

    expect(chunk1.getCode()).to.equal("123456");
    expect(chunk1.mappings.length).to.equal(2);

    expect(chunk1.mappings[0].generated.line).to.equal(1);
    expect(chunk1.mappings[0].original.line).to.equal(1);
    expect(chunk1.mappings[1].generated.line).to.equal(1);
    expect(chunk1.mappings[1].original.line).to.equal(1);

    expect(chunk1.mappings[1].generated.column).to.equal(3);
  });

  it("append onto multi-line buffer not ending in newline", async () => {
    // buffer "a\nb" (no trailing newline), appended mapping on generated line 1
    // must land on the last line of the buffer, not line 1
    const base = new Chunk().append("a", new Position(1, 1), new Dummy());
    base.appendString("\nb");
    const chunk2 = new Chunk().append("c", new Position(5, 1), new Dummy());
    base.appendChunk(chunk2);

    expect(base.getCode()).to.equal("a\nbc");
    expect(base.mappings.length).to.equal(2);
    // the appended mapping continues line 2 ("b"), at column 1
    expect(base.mappings[1].generated.line).to.equal(2);
    expect(base.mappings[1].generated.column).to.equal(1);
  });

  it("appendChunk does not mutate the source chunk (no aliasing)", async () => {
    const source = new Chunk().append("X", new Position(3, 2), new Dummy());
    expect(source.mappings[0].generated.line).to.equal(1);
    expect(source.mappings[0].generated.column).to.equal(0);

    const target = new Chunk().append("prefix", new Position(1, 1), new Dummy());
    target.appendString("\n");
    target.appendChunk(source);

    // the source chunk's own mapping must be untouched after being appended
    expect(source.mappings[0].generated.line).to.equal(1);
    expect(source.mappings[0].generated.column).to.equal(0);
  });

  it("appending the same chunk twice shifts each copy independently", async () => {
    const piece = new Chunk().append("Y", new Position(4, 3), new Dummy());

    const target = new Chunk();
    target.appendChunk(piece); // -> line 1, col 0
    target.appendChunk(piece); // -> line 1, col 1 (continues "Y")

    expect(target.getCode()).to.equal("YY");
    expect(target.mappings.length).to.equal(2);
    expect(target.mappings[0].generated.column).to.equal(0);
    expect(target.mappings[1].generated.column).to.equal(1);
    // both still map back to the same original position
    expect(target.mappings[0].original.line).to.equal(4);
    expect(target.mappings[1].original.line).to.equal(4);
  });

  it("runIndentationLogic shifts columns and tolerates unbalanced braces", async () => {
    const chunk = new Chunk().append("foo {", new Position(1, 1), new Dummy());
    chunk.appendString("\n");
    chunk.appendChunk(new Chunk().append("bar", new Position(2, 1), new Dummy()));
    chunk.appendString("\n}");

    chunk.runIndentationLogic();

    expect(chunk.getCode()).to.equal("foo {\n  bar\n}");
    // the "bar" mapping is on generated line 2, indented by 2 spaces
    const inner = chunk.mappings.find(m => m.generated.line === 2);
    expect(inner?.generated.column).to.equal(2);
  });

  it("getMap applies a generatedLineOffset", async () => {
    const chunk = new Chunk().append("code", new Position(7, 1), new Dummy());
    const consumer = await new sourceMap.SourceMapConsumer(JSON.parse(chunk.getMap("out.mjs", {generatedLineOffset: 2})));
    // the mapping was on generated line 1; with offset 2 it must decode at line 3
    const found: number[] = [];
    consumer.eachMapping(m => found.push(m.generatedLine));
    expect(found).to.include(3);
    expect(found).to.not.include(1);
  });

  it("getMap rewrites source paths", async () => {
    const chunk = new Chunk().append("code", new Position(1, 1), new Dummy());
    const map = JSON.parse(chunk.getMap("out.mjs", {sourcePaths: {dummy: "sub/dir/dummy.abap"}}));
    expect(map.sources).to.include("sub/dir/dummy.abap");
    expect(map.sources).to.not.include("dummy");
  });

});
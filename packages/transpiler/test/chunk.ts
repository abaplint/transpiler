import {expect} from "chai";
import {Position} from "@abaplint/core";
import {Chunk} from "../src/chunk";

describe("Chunk", () => {

  it("append lines", async () => {
    const chunk1 = new Chunk().appendString("START", new Position(1, 1));
    chunk1.appendString("\n");
    const chunk2 = new Chunk().appendString("END", new Position(2, 1));
    chunk1.appendChunk(chunk2);

    expect(chunk1.getCode()).to.equal("START\nEND");
    expect(chunk1.map.length).to.equal(2);

    expect(chunk1.map[0].generated.line).to.equal(1);
    expect(chunk1.map[0].original.line).to.equal(1);

    expect(chunk1.map[1].generated.line).to.equal(2);
    expect(chunk1.map[1].original.line).to.equal(2);
  });

  it("append on same line", async () => {
    const chunk1 = new Chunk().appendString("123", new Position(1, 1));
    const chunk2 = new Chunk().appendString("456", new Position(1, 4));
    chunk1.appendChunk(chunk2);

    expect(chunk1.getCode()).to.equal("123456");
    expect(chunk1.map.length).to.equal(2);

    expect(chunk1.map[0].generated.line).to.equal(1);
    expect(chunk1.map[0].original.line).to.equal(1);
    expect(chunk1.map[1].generated.line).to.equal(1);
    expect(chunk1.map[1].original.line).to.equal(1);

    expect(chunk1.map[1].generated.column).to.equal(3);
  });

});
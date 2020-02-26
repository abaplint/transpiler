import {expect} from "chai";
import { run } from "../src";

describe("Test", () => {
// todo, refactor this, so the code is not repeated
  it("DATA foo TYPE i.", () => {
    expect(run("DATA foo TYPE i.")).to.equal("let foo = new abap.basictypes.i();");
  });

  it("foo = 2.", () => {
    expect(run("foo = 2.")).to.equal("foo.set(2);");
  });

});
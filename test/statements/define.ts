import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DEFINE", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("simple", async () => {
    const code = `
DEFINE foo.
  WRITE 'hello'.
END-OF-DEFINITION.
foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("plus", async () => {
    const code = `
DEFINE foo.
  &1 = &1 + 1.
END-OF-DEFINITION.
DATA bar TYPE i.
foo bar.
WRITE bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("js keyword in macro contents", async () => {
    const code = `
DEFINE foo.
  WHILE 1 = 2.
  ENDWHILE.
END-OF-DEFINITION.

foo.
WRITE 'hello'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("js keyword in macro comment contents", async () => {
    const code = `
DEFINE foo.
*  else. offset = pos. endif.
  WHILE 1 = 2.
  ENDWHILE.
END-OF-DEFINITION.

foo.
WRITE 'hello'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("js keyword in macro, if", async () => {
    const code = `
DEFINE test.
  IF 1 = 2.
  else.
  ENDIF.
end-of-definition.

TEST.
WRITE 'hello'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("js keyword in macro, true", async () => {
    const code = `
CONSTANTS: BEGIN OF c_bool,
             true TYPE c LENGTH 1 VALUE 'X',
           END OF c_bool.

DEFINE test.
  WRITE c_bool-true.
end-of-definition.

test.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("X");
  });

});
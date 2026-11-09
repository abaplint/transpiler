import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

// the behavior definition the statements name; abaplint checks it exists
const bdef = `managed implementation in class zbp_note unique;
define behavior for zi_note alias Note
persistent table znote
lock master
{
  create;
  update;
  delete;
  action Approve result [1] $self;
}`;

async function run(contents: string) {
  return runFiles(abap, [
    {filename: "zfoobar.prog.abap", contents},
    {filename: "zi_note.bdef.asbdef", contents: bdef},
  ]);
}

/** a fake KERNEL_RAP_EML recording what the statements hand it */
class FakeKernel {
  public calls: {method: string, input: any}[] = [];
  public async begin(input: any) { this.calls.push({method: "begin", input}); }
  public async modify(input: any) { this.calls.push({method: "modify", input}); }
  public async execute(input: any) { this.calls.push({method: "execute", input}); }
  public async read(input: any) { this.calls.push({method: "read", input}); }
  public async commit(input: any) { this.calls.push({method: "commit", input}); abap.builtin.sy.get().subrc.set(0); }
  public async rollback() { this.calls.push({method: "rollback", input: undefined}); }
}

describe("Running statements - EML (MODIFY / READ / COMMIT / ROLLBACK ENTITIES)", () => {
  let kernel: FakeKernel;

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
    kernel = new FakeKernel();
    abap.Classes["KERNEL_RAP_EML"] = kernel as any;
  });

  it("hands the kernel the names, the tables and the targets of a MODIFY", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         noteid   TYPE i,
         notetext TYPE string,
       END OF ty_row.
DATA lt_create TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA lt_approve TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_mapped TYPE string.
DATA ls_failed TYPE string.
APPEND INITIAL LINE TO lt_create.
MODIFY ENTITIES OF zi_note ENTITY Note
  CREATE FIELDS ( NoteText ) WITH lt_create
  EXECUTE Approve FROM lt_approve
  MAPPED ls_mapped FAILED ls_failed.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(kernel.calls.map(c => c.method)).to.deep.equal(["begin", "modify", "execute"]);
    const begin = kernel.calls[0].input;
    expect(begin.bdef.get()).to.equal("ZI_NOTE");
    expect(begin.mapped).to.not.equal(undefined);
    expect(begin.failed).to.not.equal(undefined);
    expect(begin.reported).to.equal(undefined);
    const create = kernel.calls[1].input;
    expect(create.bdef.get()).to.equal("ZI_NOTE");
    expect(create.entity.get()).to.equal("NOTE");
    expect(create.operation.get()).to.equal("CREATE");
    expect(create.fields.get()).to.equal("NOTETEXT");
    expect(create.rows.array().length).to.equal(1);
    expect(create.mapped).to.equal(undefined);
    const approve = kernel.calls[2].input;
    expect(approve.action.get()).to.equal("APPROVE");
    expect(approve.rows.array().length).to.equal(0);
  });

  it("a RESULT target reaches the read, FAILED the statement's begin", async () => {
    const code = `
DATA lt_keys TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lt_result TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA ls_failed TYPE string.
READ ENTITIES OF zi_note ENTITY Note ALL FIELDS WITH lt_keys RESULT lt_result FAILED ls_failed.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(kernel.calls.map(c => c.method)).to.deep.equal(["begin", "read"]);
    expect(kernel.calls[0].input.failed).to.not.equal(undefined);
    const read = kernel.calls[1].input;
    expect(read.fields.get()).to.equal("*");
    expect(read.result).to.not.equal(undefined);
    expect(read.failed).to.equal(undefined);
  });

  it("COMMIT and ROLLBACK ENTITIES reach the kernel", async () => {
    const code = `
DATA ls_failed TYPE string.
DATA ls_reported TYPE string.
COMMIT ENTITIES RESPONSE OF zi_note FAILED ls_failed REPORTED ls_reported.
WRITE sy-subrc.
ROLLBACK ENTITIES.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(kernel.calls.map(c => c.method)).to.deep.equal(["commit", "rollback"]);
    expect(kernel.calls[0].input.bdef.get()).to.equal("ZI_NOTE");
    expect(abap.console.get()).to.equal("0");
  });

  it("says which kernel class is missing", async () => {
    delete abap.Classes["KERNEL_RAP_EML"];
    const js = await run(`ROLLBACK ENTITIES.`);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail("expected an error");
    } catch (e) {
      expect(String(e)).to.contain("RollbackEntities, kernel class missing");
    }
  });
});

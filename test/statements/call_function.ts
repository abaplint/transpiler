import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

describe("Running statements - CALL FUNCTION IN UPDATE TASK", () => {
  let abap: ABAP;

  beforeEach(() => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  async function run(contents: string) {
    const js = await runFiles(abap, [{filename: "zfoobar.prog.abap", contents}], {ignoreSyntaxCheck: true});
    await new AsyncFunction("abap", js)(abap);
  }

  it("without a host, runs the module at CALL time", async () => {
    abap.FunctionModules["BAR"] = async (param: any) => {
      abap.statements.write(param.exporting.foo);
    };
    await run(`
CALL FUNCTION 'BAR' IN UPDATE TASK EXPORTING foo = 42.
WRITE 'after'.`);
    expect(abap.console.get()).to.equal("42after");
  });

  it("with a host, registers the parameters without running the module", async () => {
    let registered: any;
    let calls = 0;
    abap.FunctionModules["BAR"] = async () => { calls++; };
    abap.context.updateTask = {register: (name, param) => { registered = {name, param}; }};
    await run("CALL FUNCTION 'BAR' IN UPDATE TASK EXPORTING foo = 42.");
    expect(registered.name).to.equal("BAR");
    expect(registered.param.exporting.foo.get()).to.equal(42);
    expect(calls).to.equal(0);
  });

  it("keeps registered parameters when the caller changes a variable and a table row", async () => {
    let registered: any;
    abap.FunctionModules["BAR"] = async () => { throw new Error("must not run"); };
    abap.context.updateTask = {register: (_name, param) => { registered = param; }};
    await run(`
DATA value TYPE string VALUE 'before'.
TYPES: BEGIN OF ty_row,
         value TYPE string,
       END OF ty_row.
DATA tab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA row TYPE ty_row.
FIELD-SYMBOLS <row> TYPE ty_row.
row-value = 'row before'.
APPEND row TO tab.
CALL FUNCTION 'BAR' IN UPDATE TASK EXPORTING foo = value TABLES tab = tab.
value = 'after'.
READ TABLE tab INDEX 1 ASSIGNING <row>.
<row>-value = 'row after'.`);
    expect(registered.exporting.foo.get()).to.equal("before");
    expect(registered.tables.tab.array()[0].get().value.get()).to.equal("row before");
  });

  it("raises CX_SY_DYN_CALL_ILLEGAL_FUNC at CALL time before registering", async () => {
    class IllegalFunction extends Error {
      public function: any;
      public async constructor_(param: any) {
        this.function = param.function;
        return this;
      }
    }
    abap.Classes["CX_SY_DYN_CALL_ILLEGAL_FUNC"] = IllegalFunction;
    let registrations = 0;
    abap.context.updateTask = {register: () => { registrations++; }};
    let caught: any;
    try {
      await run("CALL FUNCTION 'MISSING' IN UPDATE TASK. WRITE 'after'.");
    } catch (error) {
      caught = error;
    }
    expect(caught).to.be.instanceof(IllegalFunction);
    expect(caught.function.get()).to.equal("MISSING");
    expect(registrations).to.equal(0);
    expect(abap.console.get()).to.equal("");
  });

  it("copies what field symbols point to", async () => {
    let registered: any;
    abap.context.updateTask = {register: (_name, param) => { registered = param; }};
    abap.FunctionModules["BAR"] = async () => { throw new Error("must not run"); };
    await run(`
DATA value TYPE string VALUE 'before'.
DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
FIELD-SYMBOLS <value> TYPE string.
FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
APPEND 'row before' TO tab.
ASSIGN value TO <value>.
ASSIGN tab TO <tab>.
CALL FUNCTION 'BAR' IN UPDATE TASK EXPORTING foo = <value> TABLES tab = <tab>.
value = 'after'.
CLEAR tab.`);
    expect(registered.exporting.foo.get()).to.equal("before");
    expect(registered.tables.tab.array()[0].get()).to.equal("row before");
  });
});

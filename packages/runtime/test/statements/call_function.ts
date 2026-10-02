import {expect} from "chai";
import {ABAP} from "../../src";

describe("Statement CALL FUNCTION IN UPDATE TASK", () => {
  let abap: ABAP;

  beforeEach(() => {
    abap = new ABAP();
    (globalThis as any).abap = abap;
  });

  it("without a host, runs immediately with copied parameters", async () => {
    const value = new abap.types.String().set("before");
    let calls = 0;
    abap.FunctionModules["BAR"] = async (param: any) => {
      calls++;
      expect(param.exporting.foo.get()).to.equal("before");
      expect(param.exporting.foo).not.to.equal(value);
      param.exporting.foo.set("inside");
    };
    await abap.statements.callFunction({name: "BAR", updateTask: true, exporting: {foo: value}});
    expect(calls).to.equal(1);
    expect(value.get()).to.equal("before");
  });

  it("registers the trimmed name with a host instead of running the module", async () => {
    let calls = 0;
    let registered: any;
    abap.FunctionModules["BAR"] = async () => { calls++; };
    abap.context.updateTask = {
      register: (name, param) => { registered = {name, param}; },
    };
    const value = new abap.types.Integer().set(42);
    await abap.statements.callFunction({name: "BAR  ", updateTask: true, exporting: {foo: value},
      importing: {ignored: value}, changing: {ignored: value}});
    expect(registered.name).to.equal("BAR");
    expect(registered.param.exporting.foo.get()).to.equal(42);
    expect(registered.param).not.to.have.property("importing");
    expect(registered.param).not.to.have.property("changing");
    expect(calls).to.equal(0);
  });

  it("copies exported values, nested structures and table rows at CALL time", async () => {
    const value = new abap.types.String().set("before");
    const structure = new abap.types.Structure({nested: new abap.types.Structure({value})});
    const table = new abap.types.Table(new abap.types.Structure({value: new abap.types.String()}),
      {withHeader: false, keyType: abap.types.TableKeyType.default});
    table.append(new abap.types.Structure({value: new abap.types.String().set("row before")}));
    let registered: any;
    abap.FunctionModules["BAR"] = async () => { throw new Error("must not run"); };
    abap.context.updateTask = {register: (_name, param) => { registered = param; }};
    await abap.statements.callFunction({name: "BAR", updateTask: true,
      exporting: {value, structure}, tables: {tab: table}});
    value.set("after");
    table.array()[0].get().value.set("row after");
    expect(registered.exporting.value.get()).to.equal("before");
    expect(registered.exporting.structure.get().nested.get().value.get()).to.equal("before");
    expect(registered.tables.tab.array()[0].get().value.get()).to.equal("row before");
  });

  it("copies data and object references while keeping their referents", async () => {
    const target = new abap.types.String().set("before");
    const data = new abap.types.DataReference(target).assign(target);
    const object = {value: "before"};
    const reference = new abap.types.ABAPObject().set(object);
    let registered: any;
    abap.FunctionModules["BAR"] = async () => { throw new Error("must not run"); };
    abap.context.updateTask = {register: (_name, param) => { registered = param; }};
    await abap.statements.callFunction({name: "BAR", updateTask: true, exporting: {data, reference}});
    data.unassign();
    reference.clear();
    expect(registered.exporting.data).not.to.equal(data);
    expect(registered.exporting.data.getPointer()).to.equal(target);
    expect(registered.exporting.reference).not.to.equal(reference);
    expect(registered.exporting.reference.get()).to.equal(object);
  });

  it("awaits an asynchronous host registration", async () => {
    let finished = false;
    let release: () => void;
    const pending = new Promise<void>(resolve => { release = resolve; });
    abap.FunctionModules["BAR"] = async () => { throw new Error("must not run"); };
    abap.context.updateTask = {register: async () => {
      await pending;
      finished = true;
    }};
    const call = abap.statements.callFunction({name: "BAR", updateTask: true});
    let returned = false;
    call.then(() => { returned = true; });
    await Promise.resolve();
    expect(returned).to.equal(false);
    release!();
    await call;
    expect(finished).to.equal(true);
  });

  it("raises an illegal function exception before registering a missing module", async () => {
    class IllegalFunction extends Error {}
    abap.Classes["CX_SY_DYN_CALL_ILLEGAL_FUNC"] = IllegalFunction;
    let calls = 0;
    abap.context.updateTask = {register: () => { calls++; }};
    let caught: any;
    try {
      await abap.statements.callFunction({name: "MISSING", updateTask: true});
    } catch (error) {
      caught = error;
    }
    expect(caught).to.be.instanceof(IllegalFunction);
    expect(calls).to.equal(0);
  });
});

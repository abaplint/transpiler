import {expect} from "chai";
import {runSingle} from "./_utils";

describe("Transpile CALL FUNCTION", () => {
  const name = "'BAR'";
  const call = `abap.FunctionModules[${name}]`;
  const illegalFunc = "abap.Classes['CX_SY_DYN_CALL_ILLEGAL_FUNC'.trimEnd()]";
  // eslint-disable-next-line max-len
  const guard = `if (${call} === undefined) { if (${illegalFunc} === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_FUNC not found"; } else { throw await new ${illegalFunc}().constructor_({function: new abap.types.String().set(${name})});} }\n`;

  const tests = [
    {abap: "CALL FUNCTION 'BAR' IN UPDATE TASK.",
      js: guard + "await abap.statements.callFunction({name:'BAR',updateTask:true});"},
    {abap: "CALL FUNCTION 'BAR' IN UPDATE TASK EXPORTING foo = boo TABLES tab = itab.",
      js: guard + "await abap.statements.callFunction({name:'BAR',updateTask:true,exporting: {foo: boo}, tables: {tab: itab}});"},
    {abap: "call function 'BAR' in\nupdate task exporting foo = boo.",
      js: guard + "await abap.statements.callFunction({name:'BAR',updateTask:true,exporting: {foo: boo}});"},
    {abap: "CALL FUNCTION 'BAR' EXPORTING foo = boo.",
      js: guard + `await ${call}({exporting: {foo: boo}});`},
    {abap: "CALL FUNCTION 'BAR' EXPORTING update = task.",
      js: guard + `await ${call}({exporting: {update: task}});`},
    {abap: "CALL FUNCTION 'BAR' DESTINATION 'MOO' EXPORTING foo = boo.",
      js: "await abap.statements.callFunction({name:'BAR',destination:'MOO',exporting: {foo: boo}});"},
    {abap: "CALL FUNCTION 'BAR' STARTING NEW TASK 'foo' CALLING return_info ON END OF TASK EXPORTING foo = boo.",
      js: "abap.statements.callFunction({name:'BAR',calling:this.return_info,exporting: {foo: boo}});"},
    {abap: "CALL FUNCTION 'BAR' STARTING NEW TASK 'foo' EXPORTING foo = boo.",
      js: guard + `await ${call}({exporting: {foo: boo}});`},
  ];

  for (const test of tests) {
    it(test.abap, async () => {
      expect(await runSingle(test.abap, {ignoreSyntaxCheck: true})).to.equal(test.js);
    });
  }

  it("keeps the plain call's EXCEPTIONS handling", async () => {
    const plain = await runSingle("CALL FUNCTION 'BAR' EXCEPTIONS error = 1 OTHERS = 2.", {ignoreSyntaxCheck: true});
    const update = await runSingle("CALL FUNCTION 'BAR' IN UPDATE TASK EXCEPTIONS error = 1 OTHERS = 2.",
      {ignoreSyntaxCheck: true});
    expect(update).to.equal(plain?.replace(`await ${call}({});`, "await abap.statements.callFunction({name:'BAR',updateTask:true,});"));
  });
});

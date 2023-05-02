import {expect} from "chai";
import {UnitTestResult} from "../src/unit_test.js";

describe("xUnit XML", () => {
  it("test1", () => {
    const unit = new UnitTestResult();
    const clas = unit.addObject("ZCL_FOOBAR");
    const locl = clas.addTestClass("LCL_TEST");
    const meth = locl.addMethod("method_name");
    meth.pass();
    meth.fail();

    const xml = unit.xUnitXML();
    expect(xml).to.not.equal(undefined);
  });
});
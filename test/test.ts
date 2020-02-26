import {expect} from "chai";
import { run } from "../src";

describe("Test", () => {
  const tests = [
    {abap: "DATA foo TYPE i.", js: "let foo = new abap.basictypes.i();", skip: false},
    {abap: "foo = 2.",         js: "foo.set(2);",                        skip: false}
  ];

  for (const test of tests) {
    if (test.skip) {
      it.skip(test.abap, () => {
        expect(run(test.abap)).to.equal(test.js);
      });
    } else {
      it(test.abap, () => {
        expect(run(test.abap)).to.equal(test.js);
      });
    }
  }

});
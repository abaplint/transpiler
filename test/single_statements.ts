import {expect} from "chai";
import { run } from "../src";

describe("Single statements", () => {
  const tests = [
    {abap: "DATA foo TYPE i.",                     js: "let foo = new abap.basictypes.i();",        skip: false},
    {abap: "foo = 2.",                             js: "foo.set(2);",                               skip: false},
    {abap: "foo = bar + 2.",                       js: "foo.set(bar.add(2));",                      skip: true},
    {abap: "ADD 2 to foo.",                        js: "foo.set(foo.add(2));",                      skip: true},
    {abap: "foo = bar + moo.",                     js: "foo.set(bar.add(moo));",                    skip: true},
    {abap: "DATA foo TYPE i VALUE 2.",             js: "let foo = new abap.basictypes.i(2);",       skip: true},
    {abap: "IF foo = bar.",                        js: "if (foo.equals(bar)) {",                    skip: true},
    {abap: "ELSEIF foo = bar.",                    js: "} else if (foo.equals(bar)) {",             skip: true},
    {abap: "ELSE.",                                js: "} else {",                                  skip: true},
    {abap: "ENDIF.",                               js: "}",                                         skip: true},
    {abap: "EXIT.",                                js: "break;",                                    skip: true}, // the linter must check the EXIT is inside loop
    {abap: "CONTINUE.",                            js: "continue;",                                 skip: true}, // the linter must check the CONTINUE is inside loop
    {abap: "DATA(foo) = 2.",                       js: "let foo = new abap.basictypes.i(2);",       skip: true}, // implict type information required
    {abap: "CASE bar.",                            js: "switch (bar.get()) {",                      skip: true},
    {abap: "WHEN 2.",                              js: "case 2:",                                   skip: true}, // todo, need to add "break" in js
    {abap: "WHEN OTHERS.",                         js: "default:",                                  skip: true}, // the linter must check OTHERS is last in the CASE
    {abap: "ENDCASE.",                             js: "}",                                         skip: true},
    {abap: "DATA foo TYPE c.",                     js: "let foo = new abap.basictypes.c();",        skip: true},
    {abap: "DATA foo TYPE c LENGTH 2.",            js: "let foo = new abap.basictypes.c(2);",       skip: true},
    {abap: "DATA foo TYPE c LENGTH 2 VALUE 'fo'.", js: "let foo = new abap.basictypes.c(2, 'fo');", skip: true},
    {abap: "foo = 'fo'.",                          js: "foo.set('fo');",                            skip: true},
    {abap: "foo = |fo|.",                          js: "foo.set('fo');",                            skip: true},
    {abap: "IF foo IS INITIAL.",                   js: "if (foo.initial()) {",                      skip: true},
    {abap: "IF foo IS NOT INITIAL.",               js: "if (!foo.initial()) {",                     skip: true},
    {abap: "IF NOT foo IS INITIAL.",               js: "if (!foo.initial()) {",                     skip: true},
    {abap: "DO.",                                  js: "for (;;) {",                                skip: true}, // todo, how to set sy-fields ?
    {abap: "ENDDO.",                               js: "}",                                         skip: true},
    {abap: "DO 5 TIMES.",                          js: "for (let i = 0; i <= 5; i++) {",            skip: true}, // todo, the "i" variable must be unique
    {abap: "DO foo TIMES.",                        js: "for (let i = 0; i <= foo.get(); i++) {",    skip: true}, // todo, the "i" variable must be unique
    {abap: "LOOP AT table INTO line.",             js: "for (line of table.array()) {",             skip: true},
    {abap: "LOOP AT table INTO DATA(line).",       js: "for (let line of table.array()) {",         skip: true},
    {abap: "ENDLOOP.",                             js: "}",                                         skip: true},
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
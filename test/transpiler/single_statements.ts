import {expect} from "chai";
import {Transpiler} from "../../src/transpiler";
import {UniqueIdentifier} from "../../src/transpiler/unique_identifier";

describe("Single statements", () => {
  const tests = [
    {abap: "DATA foo TYPE i.",                     js: "let foo = new abap.types.Integer();",       skip: false},
    {abap: "foo = 2.",                             js: "foo.set(2);",                               skip: false},
    {abap: "foo = bar + 2.",                       js: "foo.set(bar.add(2));",                      skip: false},
    {abap: "ADD 2 to foo.",                        js: "foo.set(foo.add(2));",                      skip: true},
    {abap: "foo = bar + moo.",                     js: "foo.set(bar.add(moo));",                    skip: false},
    {abap: "DATA foo TYPE i VALUE 2.",             js: "let foo = new abap.types.Integer({value: 2});", skip: false},
    {abap: "IF foo = bar.",                        js: "if (foo.equals(bar)) {",                    skip: false},
    {abap: "IF foo EQ bar.",                       js: "if (foo.equals(bar)) {",                    skip: false},
    {abap: "ELSEIF foo = bar.",                    js: "} else if (foo.equals(bar)) {",             skip: false},
    {abap: "ELSE.",                                js: "} else {",                                  skip: false},
    {abap: "ENDIF.",                               js: "}",                                         skip: false},
    {abap: "EXIT.",                                js: "break;",                                    skip: false},
    {abap: "CONTINUE.",                            js: "continue;",                                 skip: false},
    {abap: "CASE bar.",                            js: "switch (bar.get()) {",                      skip: true},
    {abap: "WHEN 2.",                              js: "case 2:",                                   skip: true}, // todo, need to add "break" in JS
    {abap: "WHEN 1 OR 2.",                         js: "case 1:\ncase 2:",                          skip: true},
    {abap: "WHEN OTHERS.",                         js: "default:",                                  skip: true},
    {abap: "ENDCASE.",                             js: "}",                                         skip: false},
    {abap: "DATA foo TYPE c.",                     js: "let foo = new abap.types.Character();",     skip: false},
    {abap: "DATA foo TYPE c LENGTH 2.",            js: "let foo = new abap.types.Character({length: 2});",       skip: true},
    {abap: "DATA foo TYPE c LENGTH 2 VALUE 'fo'.", js: "let foo = new abap.types.Character({length: 2, value: 'fo'});", skip: true},
    {abap: "foo = 'fo'.",                          js: "foo.set('fo');",                            skip: true},
    {abap: "foo = |fo|.",                          js: "foo.set(`fo`);",                            skip: true},
    {abap: "foo = `fo`.",                          js: "foo.set('fo');",                            skip: true},
    {abap: "IF foo IS INITIAL.",                   js: "if (foo.initial()) {",                      skip: true},
    {abap: "IF foo IS NOT INITIAL.",               js: "if (!foo.initial()) {",                     skip: true},
    {abap: "IF NOT foo IS INITIAL.",               js: "if (!foo.initial()) {",                     skip: true},
    {abap: "DO.",                                  js: "for (;;) {",                                skip: true}, // todo, how to set sy-fields ?
    {abap: "ENDDO.",                               js: "}",                                         skip: false},
    {abap: "DO 5 TIMES.",                          js: "for (let unique1 = 0; unique1 < 5; unique1++) {",            skip: false},
    {abap: "DO foo TIMES.",                        js: "for (let unique1 = 0; unique1 < foo.get(); unique1++) {",    skip: true}, // todo, the "i" variable must be unique
    {abap: "LOOP AT table INTO line.",             js: "for (line of table.array()) {",             skip: true},
    {abap: "ENDLOOP.",                             js: "}",                                         skip: false},
    {abap: "foo-bar = 2.",                         js: "foo.bar.set(2);",                           skip: true}, // hmm, will this kind of member access work?
    {abap: "foo(1) = 'a'.",                        js: "foo.set('a', {lenth: 1});",                 skip: true},
    {abap: "foo+1 = 'a'.",                         js: "foo.set('a', {offset: 1});",                skip: true},
    {abap: "foo+1(1) = 'a'.",                      js: "foo.set('a', {offset: 1, length: 1});",     skip: true},
    {abap: "foo(bar) = 'a'.",                      js: "foo.set('a', {lenth: bar});",               skip: true},
    {abap: "CLEAR foo.",                           js: "abap.statements.clear(foo);",               skip: true},
    {abap: "ASSERT foo = bar.",                    js: "abap.statements.assert(foo.equals(bar));",  skip: true},
    {abap: "CLASS lcl_foo IMPLEMENTATION.",        js: "class lcl_foo {",                           skip: false}, // note: no code for the CLASS DEFINITION
    {abap: "ENDCLASS.",                            js: "}",                                         skip: false},
    {abap: "METHOD foo.",                          js: "foo() {",                                   skip: false}, // todo, take the abap definition and add to the js method def
    {abap: "ENDMETHOD.",                           js: "}",                                         skip: false},
    {abap: "RETURN.",                              js: "break;",                                    skip: true}, // todo, hmm?
    {abap: "foo->method().",                       js: "foo.method();",                             skip: true},
    {abap: "foo->method(1).",                      js: "foo.method(1);",                            skip: true},
    {abap: "foo->method( bar = 2 moo = 1 ).",      js: "foo.method(1, 2);",                         skip: true}, // note: the sequence of method parameters matters in JS
    {abap: "moo = foo->method().",                 js: "moo.set(foo.method());",                    skip: true},
    {abap: "FORM foo.",                            js: "function foo() {",                          skip: true},
    {abap: "ENDFORM.",                             js: "}",                                         skip: true},
  ];

  for (const test of tests) {
    if (test.skip) {
      it.skip(test.abap, () => {
        UniqueIdentifier.reset();
        expect(new Transpiler().run(test.abap)).to.equal(test.js);
      });
    } else {
      it(test.abap, () => {
        UniqueIdentifier.reset();
        expect(new Transpiler().run(test.abap)).to.equal(test.js);
      });
    }
  }

});
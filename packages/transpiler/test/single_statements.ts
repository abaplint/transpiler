import {expect} from "chai";
import {UniqueIdentifier} from "../src/unique_identifier";
import {runSingle} from "./_utils";

describe("Single statements", () => {
  const tests = [
    {abap: "DATA foo TYPE i.",                     js: "let foo = new abap.types.Integer();",       skip: false},
    {abap: "foo = 2.",                             js: "foo.set(2);",                               skip: false},
    {abap: "foo = bar + 2.",                       js: "foo.set(bar.add(2));",                      skip: false},
    {abap: "ADD 2 to foo.",                        js: "foo.set(foo.add(2));",                      skip: true},
    {abap: "foo = bar + moo.",                     js: "foo.set(bar.add(moo));",                    skip: false},
    {abap: "DATA foo TYPE i VALUE 2.",             js: "let foo = new abap.types.Integer();\nfoo.set(2);", skip: false},
    {abap: "IF foo = bar. ENDIF.",                 js: "if (abap.compare.eq(foo, bar)) {\n}",       skip: false},
    {abap: "IF foo EQ bar. ENDIF.",                js: "if (abap.compare.eq(foo, bar)) {\n}",       skip: false},
    {abap: "EXIT.",                                js: "break;",                                    skip: false},
    {abap: "CONTINUE.",                            js: "continue;",                                 skip: false},
    {abap: "CASE bar. ENDCASE.",                   js: "switch (bar.get()) {\n}",                   skip: false},
    {abap: "DATA foo TYPE c.",                     js: "let foo = new abap.types.Character();",     skip: false},
    {abap: "DATA foo TYPE string.",                js: "let foo = new abap.types.String();",        skip: false},
    {abap: "DATA foo TYPE c LENGTH 2.",            js: "let foo = new abap.types.Character({length: 2});",       skip: true},
    {abap: "DATA foo TYPE c LENGTH 2 VALUE 'fo'.", js: "let foo = new abap.types.Character({length: 2});\nfoo.set('fo');", skip: true},
    {abap: "foo = 'fo'.",                          js: "foo.set('fo');",                            skip: false},
    {abap: "foo = |fo|.",                          js: "foo.set(`fo`);",                            skip: false},
    {abap: "foo = |fo{ 2 }|.",                     js: "foo.set(`fo${2}`);",                        skip: false},
    {abap: "foo = `fo`.",                          js: "foo.set(`fo`);",                            skip: false},
    {abap: "foo = bar+1.",                         js: "foo.set(bar.get({offset: 1}));",            skip: false},
    {abap: "foo = bar(1).",                        js: "foo.set(bar.get({length: 1}));",            skip: false},
    {abap: "foo = bar+1(1).",                      js: "foo.set(bar.get({offset: 1, length: 1}));", skip: false},
    {abap: "IF foo IS INITIAL. ENDIF.",            js: "if (abap.compare.initial(foo)) {\n}",       skip: false},
    {abap: "IF foo IS NOT INITIAL. ENDIF.",        js: "if (!foo.initial()) {\n}",                  skip: true},
    {abap: "IF NOT foo IS INITIAL. ENDIF.",        js: "if (!foo.initial()) {\n}",                  skip: true},
    {abap: "DO. ENDDO.",                           js: "for (;;) {\n}",                             skip: true}, // todo, how to set sy-fields ?
    {abap: "DO 5 TIMES. ENDDO.",                   js: "for (let unique1 = 0; unique1 < 5; unique1++) {\n}",         skip: false},
    {abap: "DO foo TIMES.  ENDDO.",                js: "for (let unique1 = 0; unique1 < foo.get(); unique1++) {\n}", skip: false}, // todo, the "i" variable must be unique
    {abap: "LOOP AT table INTO line. ENDLOOP.",    js: "for (let unique1 of table.array()) {\n  line.set(unique1);\n}",          skip: false},
    {abap: "WHILE foo = bar. ENDWHILE.",           js: "while (abap.compare.eq(foo, bar)) {\n}",    skip: false},
    {abap: "foo-bar = 2.",                         js: "foo.bar.set(2);",                           skip: true}, // hmm, will this kind of member access work?
    {abap: "foo(1) = 'a'.",                        js: "foo.set('a', {lenth: 1});",                 skip: true},
    {abap: "foo+1 = 'a'.",                         js: "foo.set('a', {offset: 1});",                skip: true},
    {abap: "foo+1(1) = 'a'.",                      js: "foo.set('a', {offset: 1, length: 1});",     skip: true},
    {abap: "foo(bar) = 'a'.",                      js: "foo.set('a', {lenth: bar});",               skip: true},
    {abap: "CLEAR foo.",                           js: "abap.statements.clear(foo);",               skip: false},
    {abap: "SORT foo.",                            js: "abap.statements.sort(foo);",                skip: false},
    {abap: "WRITE foo.",                           js: "abap.statements.write(foo);",               skip: false},
    {abap: "WRITE / foo.",                         js: "abap.statements.write(foo, {newLine: true});", skip: false},
    {abap: "CLASS lcl_foo IMPLEMENTATION. ENDCLASS.", js: "class lcl_foo {\n}",                        skip: false}, // note: no code for the CLASS DEFINITION
    {abap: "RETURN.",                                 js: "break;",                                    skip: true}, // todo, hmm?
    {abap: "method( ).",                              js: "this.method();",                            skip: false},
    {abap: "foo->method( ).",                         js: "foo.get().method();",                       skip: false},
    {abap: "foo->method( 1 ).",                       js: "foo.get().method(1);",                      skip: true}, // todo, hmm, need to know the default parameter name?
    {abap: "foo->method( bar = 2 moo = 1 ).",         js: "foo.get().method({bar: 2, moo: 1});",       skip: false},
    {abap: "moo = foo->method( ).",                   js: "moo.set(foo.get().method());",              skip: false},
    {abap: "FORM foo. ENDFORM.",                      js: "function foo() {\n}",                       skip: false},
    {abap: "DATA foo TYPE STANDARD TABLE OF string.", js: "let foo = new abap.types.Table();",         skip: false},
    {abap: "lv_char = lines( lt_words ).",            js: "lv_char.set(abap.builtin.lines(lt_words));",                     skip: false},
    {abap: "SPLIT foo AT bar INTO TABLE moo.",        js: "abap.statements.split({source: foo, at: bar, target: moo});",    skip: false},
    {abap: "WRITE |moo|.",                            js: "abap.statements.write(`moo`);",                                  skip: false},
    {abap: "DELETE foo WHERE bar = 2.",               js: "abap.statements.deleteInternal(foo,{where: (i) => {return abap.compare.eq(i.bar, 2);}});", skip: false},
    {abap: "DELETE ADJACENT DUPLICATES FROM foo.",    js: "abap.statements.deleteInternal(foo,{adjacent: true});",                  skip: false},
    {abap: "* comment",                               js: "// * comment",                                                           skip: true},
    {abap: "ASSERT foo = bar.",                       js: "abap.statements.assert(abap.compare.eq(foo, bar));",                     skip: false},
    {abap: "ASSERT sy-subrc = 0.",                    js: "abap.statements.assert(abap.compare.eq(sy.get().subrc, 0));",            skip: false},
    {abap: "ASSERT 0 = 1.",                           js: "abap.statements.assert(abap.compare.eq(0, 1));",                         skip: false},
    {abap: "APPEND lv_word TO lt_letters.",           js: "abap.statements.append({source: lv_word, target: lt_letters});",         skip: false},
    {abap: "WRITE |foo{ lines( lt_words ) }bar|.",    js: "abap.statements.write(`foo${abap.builtin.lines(lt_words).get()}bar`);",  skip: false},
    {abap: "ASSERT 'a' < 'b'.",                       js: "abap.statements.assert(abap.compare.lt('a', 'b'));",   skip: false},
    {abap: "rs_response-body = 'hello'.",             js: "rs_response.get().body.set('hello');",                 skip: false},
    {abap: "TYPES foo TYPE c.",                       js: undefined,                                              skip: false}, // yes, skip TYPES
    {abap: "IF ls_request-body = ''.\nENDIF.",        js: "if (abap.compare.eq(ls_request.get().body, '')) {\n}", skip: false},
    {abap: "CONCATENATE 'foo' 'bar' INTO target.",    js: "abap.statements.concatenate({source: ['foo','bar'], target: target});", skip: false},
    {abap: "zcl_bar=>do_something( ).",               js: "zcl_bar.do_something();",                  skip: false},
    {abap: "SET BIT foo OF bar.",                     js: "abap.statements.setBit(foo, bar);",        skip: false},
    {abap: "SET BIT foo OF bar TO moo.",              js: "abap.statements.setBit(foo, bar, moo);",   skip: false},
    {abap: "GET BIT foo OF bar INTO moo.",            js: "abap.statements.getBit(foo, bar, moo);",   skip: false},
    {abap: "WRITE sy-index.",                         js: "abap.statements.write(sy.get().index);",   skip: false},

  ];

  for (const test of tests) {
    if (test.skip) {
      it.skip(test.abap, async () => {
        return;
      });
    } else {
      it(test.abap, async () => {
        UniqueIdentifier.reset();
        expect(await runSingle(test.abap, {ignoreSyntaxCheck: true})).to.equal(test.js);
      });
    }
  }

});
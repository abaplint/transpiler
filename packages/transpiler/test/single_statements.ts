/* eslint-disable max-len */
import {expect} from "chai";
import {ITranspilerOptions} from "../src/types";
import {UniqueIdentifier} from "../src/unique_identifier";
import {runSingle} from "./_utils";

describe("Single statements", () => {
  const tests = [
    {abap: "BREAK-POINT.",                         js: `debugger;`,       skip: false, only: false},
    {abap: "BREAK username.",                      js: `debugger;`,       skip: false, only: false},
    {abap: "DATA foo TYPE i.",                     js: `let foo = new abap.types.Integer({qualifiedName: "I"});`,       skip: false, only: false},
    {abap: "DATA ref TYPE REF TO i.",              js: `let ref = new abap.types.DataReference(new abap.types.Integer({qualifiedName: "I"}));`,       skip: false},
    {abap: "foo = 2.",                             js: "foo.set(new abap.types.Integer().set(2));",                      skip: false},
    {abap: "foo = bar + 2.",                       js: "foo.set(abap.operators.add(bar,new abap.types.Integer().set(2)));",             skip: false},
    {abap: "foo = bar - 2.",                       js: "foo.set(abap.operators.minus(bar,new abap.types.Integer().set(2)));",           skip: false},
    {abap: "foo = bar * 2.",                       js: "foo.set(abap.operators.multiply(bar,new abap.types.Integer().set(2)));",        skip: false},
    {abap: "foo = bar / 2.",                       js: "foo.set(abap.operators.divide(bar,new abap.types.Integer().set(2)));",          skip: false},
    {abap: "foo = bar + moo.",                     js: "foo.set(abap.operators.add(bar,moo));",                    skip: false},
    {abap: "DATA foo TYPE i VALUE 2.",             js: `let foo = new abap.types.Integer({qualifiedName: "I"});\nfoo.set(2);`, skip: false},
    {abap: "IF foo = bar. ENDIF.",                 js: "if (abap.compare.eq(foo, bar)) {\n}",       skip: false},
    {abap: "IF foo EQ bar. ENDIF.",                js: "if (abap.compare.eq(foo, bar)) {\n}",       skip: false},
    {abap: "IF foo CP 'bar*'. ENDIF.",             js: "if (abap.compare.cp(foo, new abap.types.Character(4).set('bar*'))) {\n}",    skip: false},
    {abap: "IF item IS INITIAL AND NOT ( foo = bar AND bar = moo ). ENDIF.",             js: "if (abap.compare.initial(item) && !(abap.compare.eq(foo, bar) && abap.compare.eq(bar, moo))) {\n}",    skip: false},
    {abap: "CONTINUE.",                            js: "continue;",                                 skip: false},
    {abap: "IMPORT variscreens = lt_variscreens FROM MEMORY ID '%_SCRNR_%'.",                       js: `throw new Error("Import, transpiler todo");`,                                 skip: false},
    {abap: "CASE bar. ENDCASE.",                   js: "let unique1 = bar;",                        skip: false},
    {abap: "DATA foo TYPE c.",                     js: "let foo = new abap.types.Character(1, {});",     skip: false},
    {abap: "DATA foo TYPE string.",                js: `let foo = new abap.types.String({qualifiedName: "STRING"});`,      skip: false},
    {abap: "DATA foo TYPE c LENGTH 2.",            js: "let foo = new abap.types.Character(2});",                 skip: true},
    {abap: "DATA foo TYPE c LENGTH 2 VALUE 'fo'.", js: "let foo = new abap.types.Character(2);\nfoo.set('fo');", skip: true},
    {abap: "DATA bar TYPE p LENGTH 4.",            js: "let bar = new abap.types.Packed({length: 4, decimals: 0});",       skip: false},
    {abap: "foo = 'fo'.",                          js: "foo.set(new abap.types.Character(2).set('fo'));",                         skip: false},
    {abap: "foo = 'moo'(005).",                    js: "foo.set(new abap.types.Character(3).set('moo'));",                        skip: false},
    {abap: "foo = |fo|.",                          js: "foo.set(new abap.types.String().set(`fo`));",                                       skip: false},
    {abap: "foo = |fo{ 2 }|.",                     js: "foo.set(new abap.types.String().set(`fo${abap.templateFormatting(new abap.types.Integer().set(2))}`));", skip: false},
    {abap: "foo = `fo`.",                          js: "foo.set(new abap.types.String().set(`fo`));",     skip: false},
    {abap: "foo = bar+1.",                         js: "foo.set(bar.getOffset({offset: 1}));",            skip: false},
    {abap: "foo = bar(1).",                        js: "foo.set(bar.getOffset({length: 1}));",            skip: false},
    {abap: "foo = bar+1(1).",                      js: "foo.set(bar.getOffset({offset: 1, length: 1}));", skip: false},
    {abap: "IF foo IS INITIAL. ENDIF.",            js: "if (abap.compare.initial(foo)) {\n}",             skip: false},
    {abap: "IF foo IS NOT INITIAL. ENDIF.",        js: "if (abap.compare.initial(foo) === false) {\n}",   skip: false},
    {abap: "IF NOT foo IS INITIAL. ENDIF.",        js: "if (abap.compare.initial(foo) === false) {\n}",   skip: false},
    {abap: "DO. ENDDO.",                           js: "for (;;) {\n}",                                   skip: true},
    {abap: "foo-bar = 2.",                         js: "foo.bar.set(2);",                           skip: true}, // hmm, will this kind of member access work?
    {abap: "CLEAR foo.",                           js: "abap.statements.clear(foo);",               skip: false},
    {abap: "FREE foo.",                            js: "abap.statements.clear(foo);",               skip: false},
    {abap: "SORT foo.",                            js: "abap.statements.sort(foo,{});",                  skip: false},
    {abap: "SORT foo DESCENDING.",                 js: "abap.statements.sort(foo,{descending: true});",  skip: false},
    {abap: "SORT foo BY field.",                   js: `abap.statements.sort(foo,{by: [{component: "field"}]});`,  skip: false},
    {abap: "SORT foo BY field1 field2.",           js: `abap.statements.sort(foo,{by: [{component: "field1"},{component: "field2"}]});`, skip: false},
    {abap: "SORT ct_matches BY offset length DESCENDING.", js: `abap.statements.sort(ct_matches,{by: [{component: "offset"},{component: "length", descending: true}]});`, skip: false},
    {abap: "WRITE foo.",                           js: "abap.statements.write(foo);",                    skip: false},
    {abap: "WRITE / foo.",                         js: "abap.statements.write(foo,{newLine: true});", skip: false},
    {abap: "INSERT 5 INTO tab INDEX sy-tabix.", js: "abap.statements.insertInternal({data: new abap.types.Integer().set(5), index: abap.builtin.sy.get().tabix, table: tab});", skip: false},
    {abap: "RETURN.",                                 js: "return;",                                   skip: false}, // todo, hmm? some more to be added here
    {abap: "method( ).",                              js: "await this.method();",                            skip: false},
    {abap: "foo->method( ).",                         js: "await foo.get().method();",                       skip: false},
    {abap: "foo->method( 1 ).",                       js: "await foo.get().method(new abap.types.Integer().set(1));",             skip: true}, // todo, hmm, need to know the default parameter name?
    {abap: "foo->method( bar = 2 moo = 1 ).",         js: "await foo.get().method({bar: new abap.types.Integer().set(2), moo: new abap.types.Integer().set(1)});",       skip: false},
    {abap: "moo = foo->method( ).",                   js: "moo.set((await foo.get().method()));",              skip: false},
    {abap: "FORM foo. ENDFORM.",                      js: "async function foo(INPUT) {\n}",                       skip: false},
    {abap: "PERFORM foo.",                            js: "await foo({});",                       skip: false},
    {abap: "PERFORM foo IN PROGRAM zsdfsd.",                            js: `throw new Error("PerformTranspiler IN PROGRAM, transpiler todo");`,                       skip: false},
    {abap: "PERFORM foo in program zsdfsd.",                            js: `throw new Error("PerformTranspiler IN PROGRAM, transpiler todo");`,                       skip: false},
    {abap: "PERFORM ('ASDF') IN PROGRAM ('ASDF') TABLES bar.",      js: `throw new Error("PerformTranspiler FormName not found");`,                       skip: false},
    {abap: "DATA foo TYPE STANDARD TABLE OF string.",
      js: `let foo = abap.types.TableFactory.construct(new abap.types.String({qualifiedName: "STRING"}), {"withHeader":false,"keyType":"USER","primaryKey":{"name":"primary_key","type":"STANDARD","isUnique":false,"keyFields":[]},"secondary":[]}, "");`,         skip: false},
    {abap: "SPLIT foo AT bar INTO TABLE moo.",            js: "abap.statements.split({source: foo, at: bar, table: moo});",    skip: false},
    {abap: "SPLIT |blah| AT '.' INTO lv_major lv_minor.", js: "abap.statements.split({source: new abap.types.String().set(`blah`), at: new abap.types.Character(1).set('.'), targets: [lv_major,lv_minor]});",    skip: false},
    {abap: "WRITE |moo|.",                            js: "abap.statements.write(new abap.types.String().set(`moo`));",                                  skip: false},
    {abap: "DELETE foo WHERE bar = 2.",               js: "await abap.statements.deleteInternal(foo,{where: (I) => {return abap.compare.eq(I.bar, new abap.types.Integer().set(2));}});", skip: false},
    {abap: "DELETE ADJACENT DUPLICATES FROM foo.",    js: "await abap.statements.deleteInternal(foo,{adjacent: true});",          skip: false},
    {abap: "DELETE foo INDEX 2.",                     js: "await abap.statements.deleteInternal(foo,{index: new abap.types.Integer().set(2)});",       skip: false},
    {abap: "DELETE TABLE tab FROM <bar>.",            js: "await abap.statements.deleteInternal(tab,{fromValue: fs_bar_});",           skip: false},
    {abap: "* comment",                               js: "// * comment",                                                   skip: true},
    {abap: "ASSERT foo = bar.",                       js: "abap.statements.assert(abap.compare.eq(foo, bar));",             skip: false},
    {abap: "ASSERT sy-subrc = 0.",                    js: "abap.statements.assert(abap.compare.eq(abap.builtin.sy.get().subrc, new abap.types.Integer().set(0)));",            skip: false},
    {abap: "ASSERT 0 = 1.",                           js: "abap.statements.assert(abap.compare.eq(new abap.types.Integer().set(0), new abap.types.Integer().set(1)));",                         skip: false},
    {abap: "APPEND lv_word TO lt_letters.",           js: "abap.statements.append({source: lv_word, target: lt_letters});",         skip: false},
    {abap: "WRITE |foo{ lines( lt_words ) }bar|.",    js: "abap.statements.write(new abap.types.String().set(`foo${abap.templateFormatting(abap.builtin.lines({val: lt_words}))}bar`));",  skip: false},
    {abap: "ASSERT 'a' < 'b'.",                       js: "abap.statements.assert(abap.compare.lt(new abap.types.Character(1).set('a'), new abap.types.Character(1).set('b')));",    skip: false},
    {abap: "rs_response-body = 'hello'.",             js: "rs_response.get().body.set(new abap.types.Character(5).set('hello'));",                  skip: false},
    {abap: "TYPES foo TYPE c.",                       js: "",                                                      skip: false}, // yes, skip TYPES
    {abap: "IF ls_request-body = ''.\nENDIF.",        js: "if (abap.compare.eq(ls_request.get().body, new abap.types.Character(1).set(''))) {\n}",  skip: false},
    {abap: "CONCATENATE 'foo' 'bar' INTO target.",    js: "abap.statements.concatenate({source: [new abap.types.Character(3).set('foo'), new abap.types.Character(3).set('bar')], target: target});", skip: false},
    {abap: "CONCATENATE foo bar INTO tg SEPARATED BY space.",    js: "abap.statements.concatenate({source: [foo, bar], target: tg, separatedBy: abap.builtin.space});", skip: false},
    {abap: "zcl_bar=>do_something( ).",               js: "await abap.Classes['ZCL_BAR'].do_something();",                               skip: false},
    {abap: "SET BIT foo OF bar.",                     js: "abap.statements.setBit(foo, bar);",                     skip: false},
    {abap: "SET BIT foo OF bar TO moo.",              js: "abap.statements.setBit(foo, bar, moo);",                skip: false},
    {abap: "GET BIT foo OF bar INTO moo.",            js: "abap.statements.getBit(foo, bar, moo);",                skip: false},
    {abap: "WRITE sy-index.",                         js: "abap.statements.write(abap.builtin.sy.get().index);",   skip: false},
    {abap: "FIELD-SYMBOLS <bar> TYPE i.",             js: `let fs_bar_ = new abap.types.FieldSymbol(new abap.types.Integer({qualifiedName: "I"}));`, skip: false},
    {abap: "ASSIGN da TO <name>.",                    js: "abap.statements.assign({target: fs_name_, source: da});",        skip: false},
    {abap: "ASSIGN <fs1> TO <fs2>.",                  js: "abap.statements.assign({target: fs_fs2_, source: fs_fs1_});",    skip: false},
    {abap: "ASSERT <name> = 1.",                      js: "abap.statements.assert(abap.compare.eq(fs_name_, new abap.types.Integer().set(1)));", skip: false},
    {abap: "<name> = 1.",                             js: "fs_name_.set(new abap.types.Integer().set(1));",                                      skip: false},
    {abap: "CONSTANTS c TYPE i VALUE 1.",             js: `let c = new abap.types.Integer({qualifiedName: "I"});\nc.set(1);`,           skip: false},
    {abap: "READ TABLE tab INDEX i INTO target.",          js: `abap.statements.readTable(tab,{index: i,
  into: target});`, skip: false},
    {abap: "READ TABLE tab INTO line WITH KEY field = 2.", js: `abap.statements.readTable(tab,{into: line,
  withKey: (i) => {return abap.compare.eq(i.field, new abap.types.Integer().set(2));},
  withKeyValue: [{key: (i) => {return i.field}, value: new abap.types.Integer().set(2)}],
  usesTableLine: false,
  withKeySimple: {"field": new abap.types.Integer().set(2)}});`, skip: false},
    {abap: "READ TABLE tab INDEX i ASSIGNING <nam>.",      js: `abap.statements.readTable(tab,{index: i,
  assigning: fs_nam_});`,   skip: false},
    {abap: "READ TABLE <lt_indices> WITH KEY ('PROGRAM_NDX') = lv_program_ndx ASSIGNING <ls_alert_by_index>.",
      js: `abap.statements.readTable(fs_lt_indices_,{assigning: fs_ls_alert_by_index_,
  withKey: (i) => {return abap.compare.eq(i.program_ndx, lv_program_ndx);},
  withKeyValue: [{key: (i) => {return i.program_ndx}, value: lv_program_ndx}],
  usesTableLine: false,
  withKeySimple: {"program_ndx": lv_program_ndx}});`,   skip: false},

    {abap: "MODIFY result INDEX 1 FROM 4.",           js: "abap.statements.modifyInternal(result,{index: new abap.types.Integer().set(1),from: new abap.types.Integer().set(4)});",   skip: false},
    {abap: "WRITE |foo| && |bar|.",                   js: "abap.statements.write(abap.operators.concat(new abap.types.String().set(`foo`),new abap.types.String().set(`bar`)));",                 skip: false},
    {abap: "WRITE zcl_name=>c_maxbits.",              js: "abap.statements.write(abap.Classes['ZCL_NAME'].c_maxbits);",            skip: false},
    {abap: "WRITE |`|.",                              js: "abap.statements.write(new abap.types.String().set(`\\``));",                         skip: false},
    {abap: "ASSERT NOT act IS INITIAL.",              js: "abap.statements.assert(abap.compare.initial(act) === false);", skip: false},
    {abap: "* comment",                               js: "",                    skip: false},
    {abap: "\" comment",                              js: "",                    skip: false},
    {abap: "WRITE '@KERNEL let arr = 2;'.",           js: "let arr = 2;",        skip: false},
    {abap: "WRITE foo->bar.",                         js: "abap.statements.write(foo.get().bar);",        skip: false},
    {abap: "type->type_kind = 2.",                    js: "type.get().type_kind.set(new abap.types.Integer().set(2));",         skip: false},
    {abap: "REPLACE ALL OCCURRENCES OF |\\n| IN lv_norm WITH | |.",              js: `abap.statements.replace({target: lv_norm, all: true, with: new abap.types.String().set(\` \`), of: new abap.types.String().set(\`\\n\`)});`,         skip: false},
    {abap: "CONDENSE lv_norm.",                                                  js: "abap.statements.condense(lv_norm, {nogaps: false});",         skip: false},
    {abap: "FIND FIRST OCCURRENCE OF |bar| IN |foobar| MATCH OFFSET lv_offset.",
      js: "abap.statements.find(new abap.types.String().set(`foobar`), {find: new abap.types.String().set(`bar`), first: true, offset: lv_offset});", skip: false},
    {abap: "FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_string.",
      js: `abap.statements.find(iv_string, {find: abap.Classes['CL_ABAP_CHAR_UTILITIES'].cr_lf, first: true});`, skip: false},
    {abap: "FIND FIRST OCCURRENCE OF REGEX 'b+c' IN 'abcd' MATCH COUNT lv_cnt MATCH LENGTH lv_len.",
      js: "abap.statements.find(new abap.types.Character(4).set('abcd'), {regex: new abap.types.Character(3).set('b+c'), first: true, count: lv_cnt, length: lv_len});", skip: false},
    {abap: "FIND REGEX '11(\\w+)22' IN '11abc22' SUBMATCHES lv_host.",
      js: "abap.statements.find(new abap.types.Character(7).set('11abc22'), {regex: new abap.types.Character(9).set('11(\\\\w+)22'), submatches: [lv_host]});", skip: false},
    {abap: "SHIFT lv_bitbyte LEFT DELETING LEADING '0 '.", js: `abap.statements.shift(lv_bitbyte, {direction: 'LEFT',deletingLeading: new abap.types.Character(2).set('0 ')});`, skip: false},
    {abap: "SHIFT lv_temp BY 1 PLACES LEFT.", js: `abap.statements.shift(lv_temp, {direction: 'LEFT',places: new abap.types.Integer().set(1)});`, skip: false},
    {abap: "SHIFT lv_temp UP TO '/' LEFT.", js: `abap.statements.shift(lv_temp, {direction: 'LEFT',to: new abap.types.Character(1).set('/')});`, skip: false},
    {abap: "TRANSLATE rv_spras TO UPPER CASE.", js: `abap.statements.translate(rv_spras, "UPPER");`, skip: false},
    {abap: "TRANSLATE rv_spras TO LOWER CASE.", js: `abap.statements.translate(rv_spras, "LOWER");`, skip: false},
    {abap: "DESCRIBE FIELD <lg_line> LENGTH lv_length IN CHARACTER MODE.", js: `abap.statements.describe({field: fs_lg_line_, length: lv_length, mode: 'CHARACTER'});`, skip: false},
    {abap: "DESCRIBE FIELD <lg_line> LENGTH lv_length IN BYTE MODE.", js: `abap.statements.describe({field: fs_lg_line_, length: lv_length, mode: 'BYTE'});`, skip: false},
    {abap: "DESCRIBE FIELD tab TYPE type.", js: `abap.statements.describe({field: tab, type: type});`,    skip: false},
    {abap: "foo = 2 ** 2.",   js: `foo.set(abap.operators.power(new abap.types.Integer().set(2),new abap.types.Integer().set(2)));`,                skip: false},
    {abap: "foo = 5 DIV 2.",  js: `foo.set(abap.operators.div(new abap.types.Integer().set(5),new abap.types.Integer().set(2)));`,                  skip: false},
    {abap: "foo+5(1) = 'A'.", js: `new abap.OffsetLength(foo, {offset: 5, length: 1}).set(new abap.types.Character(1).set('A'));`, skip: false},
    {abap: "foo(1) = 'a'.",   js: "new abap.OffsetLength(foo, {length: 1}).set(new abap.types.Character(1).set('a'));",            skip: false},
    {abap: "foo+1 = 'a'.",    js: "new abap.OffsetLength(foo, {offset: 1}).set(new abap.types.Character(1).set('a'));",            skip: false},
    {abap: "foo+1(1) = 'a'.", js: "new abap.OffsetLength(foo, {offset: 1, length: 1}).set(new abap.types.Character(1).set('a'));", skip: false},
    {abap: "foo(bar) = 'a'.", js: "new abap.OffsetLength(foo, {length: bar}).set(new abap.types.Character(1).set('a'));",    skip: false},
    {abap: "IF iv_cd = '' OR iv_cd = '.'.\nENDIF.", js: "if (abap.compare.eq(iv_cd, new abap.types.Character(1).set('')) || abap.compare.eq(iv_cd, new abap.types.Character(1).set('.'))) {\n}", skip: false},
    {abap: "TRY. ENDTRY.", js: ``,    skip: false, only: false},
    {abap: "MESSAGE e058(00) WITH 'Value_1' 'Value_2' 'Value_3' 'Value_4' INTO lv_dummy.",
      js: `await abap.statements.message({into: lv_dummy, id: "00", number: "058", type: "E", with: [new abap.types.Character(7).set('Value_1'),new abap.types.Character(7).set('Value_2'),new abap.types.Character(7).set('Value_3'),new abap.types.Character(7).set('Value_4')]});`, skip: false},
    {abap: "MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO rv_text.",
      js: `await abap.statements.message({into: rv_text, id: abap.builtin.sy.get().msgid, type: new abap.types.Character(1).set('S'), number: abap.builtin.sy.get().msgno, with: [abap.builtin.sy.get().msgv1,abap.builtin.sy.get().msgv2,abap.builtin.sy.get().msgv3,abap.builtin.sy.get().msgv4]});`, skip: false},
    {abap: "RAISE EXCEPTION instance.", js: `throw instance.get();`, skip: false},
    {abap: "CLASS ltcl_test DEFINITION DEFERRED.", js: ``, skip: false},
    {abap: "CLASS sdfsdf DEFINITION LOCAL FRIENDS ltcl_test ltcl_split_text.", js: ``, skip: false},
    {abap: "WAIT UP TO 1 SECONDS.", js: `await new Promise(r => setTimeout(r, new abap.types.Integer().set(1).get() * 1000));`, skip: false},
    {abap: "if_bar~field = 2.",                      js: `if_bar$field.set(new abap.types.Integer().set(2));`, skip: false},
    {abap: "IF if_bar~field IS NOT INITIAL. ENDIF.", js: `if (abap.compare.initial(if_bar$field) === false) {\n}`, skip: false},
    {abap: "FUNCTION-POOL zopenabap.", js: ``, skip: false},
    {abap: "INCLUDE lzopenabaptop.", js: ``, skip: false},

    {abap: "CALL FUNCTION 'BAR' DESTINATION 'MOO'.",
      js: `await abap.statements.callFunction({name:'BAR',destination:'MOO'});`, skip: false},
    {abap: `CALL FUNCTION 'BAR' DESTINATION 'MOO' EXPORTING foo = boo.`,
      js: `await abap.statements.callFunction({name:'BAR',destination:'MOO',exporting: {foo: boo}});`, skip: false},
    {abap: `CALL FUNCTION 'BAR' STARTING NEW TASK 'foo' CALLING return_info ON END OF TASK EXPORTING foo = boo.`,
      js: `abap.statements.callFunction({name:'BAR',calling:this.return_info,exporting: {foo: boo}});`, skip: false},

    {abap: `RECEIVE RESULTS FROM FUNCTION 'BAR' IMPORTING param = val.`,
      js: `abap.statements.receive({name:'BAR',importing: {param: val}});`, skip: false},
    {abap: `RECEIVE RESULTS FROM FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
      IMPORTING
        ev_result             = lv_result
        ev_path               = lv_path
      EXCEPTIONS
        error                 = 1
        system_failure        = 2 MESSAGE lv_mess
        communication_failure = 3 MESSAGE lv_mess
        OTHERS = 4.`,
    js: `abap.statements.receive({name:'Z_ABAPGIT_SERIALIZE_PARALLEL',importing: {ev_result: lv_result, ev_path: lv_path}});`},

    {abap: "super->method( ).",      js: `await super.method();`, skip: false},
    {abap: "super->constructor( ).", js: `await super.constructor_();`, skip: false},

    {abap: "SELECT SINGLE * FROM t100 INTO ls_result.",
      js: `await abap.statements.select(ls_result, {select: "SELECT * FROM " + abap.buildDbTableName("t100") + " UP TO 1 ROWS"});`},
    {abap: "SELECT * FROM t100 INTO TABLE lt_result ORDER BY msgnr.",
      js: `await abap.statements.select(lt_result, {select: "SELECT * FROM " + abap.buildDbTableName("t100") + " ORDER BY msgnr"});`},
    {abap: "SELECT * FROM t100 INTO TABLE lt_result UP TO 2 ROWS.",
      js: `await abap.statements.select(lt_result, {select: "SELECT * FROM " + abap.buildDbTableName("t100") + " UP TO " + new abap.types.Integer().set(2).get() + " ROWS"});`},
    {abap: "SELECT * FROM t100 INTO TABLE lt_result WHERE msgnr = '123'.",
      js: `await abap.statements.select(lt_result, {select: "SELECT * FROM " + abap.buildDbTableName("t100") + " WHERE msgnr = '123'"});`},
    {abap: "SELECT * FROM (mv_table) INTO TABLE lt_tab.",
      js: `await abap.statements.select(lt_tab, {select: "SELECT * FROM " + abap.buildDbTableName(mv_table.get().trimEnd().toLowerCase()) + ""});`},
    {abap: "SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = lv_arbgb.",
      js: `await abap.statements.select(ls_result, {select: "SELECT * FROM " + abap.buildDbTableName("t100") + " WHERE arbgb = '" + lv_arbgb.get() + "' UP TO 1 ROWS"});`},
    {abap: "select count( * ) from /foo/bar into bar.",
      js: `await abap.statements.select(bar, {select: "SELECT count( * )  from " + abap.buildDbTableName("/foo/bar") + ""});`},
    {abap: "SELECT * FROM sflight INTO CORRESPONDING FIELDS OF TABLE mt_sflight UP TO 10 ROWS ORDER BY carrid.",
      js: `await abap.statements.select(mt_sflight, {select: "SELECT * FROM " + abap.buildDbTableName("sflight") + " UP TO " + new abap.types.Integer().set(10).get() + " ROWS ORDER BY carrid"});`},
    {abap: "SELECT * FROM sflight INNER JOIN scarr AS carrier ON carrier~carrid = sflight~carrid INTO CORRESPONDING FIELDS OF TABLE mt_sflight_join WHERE sflight~carrid = 'AA' ORDER BY sflight~fldate.",
      js: `await abap.statements.select(mt_sflight_join, {select: "SELECT * FROM " + abap.buildDbTableName("sflight") + " INNER JOIN " + abap.buildDbTableName("scarr") + " AS carrier ON carrier~carrid = sflight~carrid WHERE sflight~carrid = 'AA' ORDER BY sflight~fldate"});`},
    {abap: "SELECT * FROM t100 APPENDING CORRESPONDING FIELDS OF TABLE res WHERE arbgb = 'ZAG_UNIT_TEST'.",
      js: `await abap.statements.select(res, {select: "SELECT * FROM " + abap.buildDbTableName("t100") + " WHERE arbgb = 'ZAG_UNIT_TEST'"}, {appending: true});`},

    {abap: "INSERT INTO zopentest VALUES ls_row.", js: `await abap.statements.insertDatabase("zopentest", {"values": ls_row});`},
    {abap: "INSERT t100 FROM ls_t100.", js: `await abap.statements.insertDatabase("t100", {"values": ls_t100});`},
    {abap: "ASSERT NOT foo EQ bar.",     js: `abap.statements.assert(!abap.compare.eq(foo, bar));`, skip: false},
    {abap: "GET REFERENCE OF blah INTO ref.", js: `ref.assign(blah);`, skip: false},
    {abap: `GET REFERENCE OF <item> INTO lr_stack_top.`, js: `lr_stack_top.assign(fs_item_.getPointer());`, skip: false},
    {abap: "CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_timestamp TIME ZONE lv_utc.",
      js: `abap.statements.convert({date: lv_date,time: lv_time,zone: lv_utc}, {stamp: lv_timestamp});`, skip: false},
    {abap: "CONVERT TIME STAMP iv_val TIME ZONE lv_utc INTO DATE lv_date TIME lv_time.",
      js: `abap.statements.convert({stamp: iv_val,zone: lv_utc}, {date: lv_date,time: lv_time});`, skip: false},
    {abap: "COMMIT WORK.",     js: `abap.statements.commit();`, skip: false},
    {abap: `INSERT 1 INTO TABLE tab.`, js: `abap.statements.insertInternal({data: new abap.types.Integer().set(1), table: tab});`, skip: false},
    {abap: "ROLLBACK WORK.",   js: `abap.statements.rollback();`, skip: false},
    {abap: "MOVE-CORRESPONDING foo TO bar.", js: `abap.statements.moveCorresponding(foo, bar);`, skip: false},
    {abap: "ASSERT 5 IN bar.", js: `abap.statements.assert(abap.compare.in(new abap.types.Integer().set(5), bar));`, skip: false},
    {abap: "INSERT INITIAL LINE INTO tab ASSIGNING <row> INDEX 1.", js: `abap.statements.insertInternal({initial: true, index: new abap.types.Integer().set(1), assigning: fs_row_, table: tab});`, skip: false},
    {abap: "DELETE lt_log_temp WHERE msg-level < iv_min_level.", js: `await abap.statements.deleteInternal(lt_log_temp,{where: (I) => {return abap.compare.lt(I.msg.get().level, iv_min_level);}});`, skip: false},
    {abap: "ASSIGN lv_x TO <lv_y> CASTING.", js: `abap.statements.assign({target: fs_lv_y_, source: lv_x, casting: true});`, skip: false},

    {abap: `CALL TRANSFORMATION id
    OPTIONS value_handling = 'accept_data_loss'
    SOURCE XML iv_string
    RESULT data = rs_xml.`, js: `if (abap.Classes['KERNEL_CALL_TRANSFORMATION'] === undefined) throw new Error("CallTransformation, kernel class missing");
await abap.Classes['KERNEL_CALL_TRANSFORMATION'].call({name: "id",sourceXML: iv_string,options: {value_handling:new abap.types.Character(16).set('accept_data_loss')},result: {data:rs_xml}});`},
    {abap: `CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE data = is_data
      RESULT XML rv_xml.`, js: `if (abap.Classes['KERNEL_CALL_TRANSFORMATION'] === undefined) throw new Error("CallTransformation, kernel class missing");
await abap.Classes['KERNEL_CALL_TRANSFORMATION'].call({name: "id",resultXML: rv_xml,options: {initial_components:new abap.types.Character(8).set('suppress')},source: {data:is_data}});`},
    {abap: `CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML mi_xml_doc
      RESULT (lt_rtab).`, js: `if (abap.Classes['KERNEL_CALL_TRANSFORMATION'] === undefined) throw new Error("CallTransformation, kernel class missing");
await abap.Classes['KERNEL_CALL_TRANSFORMATION'].call({name: "id",sourceXML: mi_xml_doc,options: {value_handling:new abap.types.Character(16).set('accept_data_loss')},result: (lt_rtab)});`},
    {abap: `CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE (lt_stab)
      RESULT XML li_doc.`, js: `if (abap.Classes['KERNEL_CALL_TRANSFORMATION'] === undefined) throw new Error("CallTransformation, kernel class missing");
await abap.Classes['KERNEL_CALL_TRANSFORMATION'].call({name: "id",resultXML: li_doc,options: {initial_components:new abap.types.Character(8).set('suppress')},source: (lt_stab)});`},
    {abap: `CALL TRANSFORMATION id
SOURCE
  data   = <fs>
  fields = fields
RESULT XML writer.`, js: `if (abap.Classes['KERNEL_CALL_TRANSFORMATION'] === undefined) throw new Error("CallTransformation, kernel class missing");
await abap.Classes['KERNEL_CALL_TRANSFORMATION'].call({name: "id",resultXML: writer,source: {data:fs_fs_,fields:fields}});`},

    {abap: "SCAN ABAP-SOURCE src TOKENS INTO tokens STATEMENTS INTO statements.", js: `if (abap.Classes['KERNEL_SCAN_ABAP_SOURCE'] === undefined) throw new Error("ScanAbapSource, kernel class missing");
await abap.Classes['KERNEL_SCAN_ABAP_SOURCE'].call({scan_abap_source: src, tokens_into: tokens, statements_into: statements});`},
    {abap: `SCAN ABAP-SOURCE source
  TOKENS INTO tokens
  STATEMENTS INTO statements
  WITH ANALYSIS
  WITH COMMENTS
  WITH PRAGMAS '*'.`, js: `if (abap.Classes['KERNEL_SCAN_ABAP_SOURCE'] === undefined) throw new Error("ScanAbapSource, kernel class missing");
await abap.Classes['KERNEL_SCAN_ABAP_SOURCE'].call({scan_abap_source: source, tokens_into: tokens, statements_into: statements, with_analysis: true, with_comments: true, with_pragmas: new abap.types.Character(1).set('*')});`},

    {abap: `DATA tab TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.`,
      js: `let tab = abap.types.TableFactory.construct(new abap.types.Integer({qualifiedName: "I"}), {"withHeader":false,"keyType":"USER","primaryKey":{"name":"primary_key","type":"SORTED","isUnique":true,"keyFields":["TABLE_LINE"]},"secondary":[]}, "");`},
    {abap: `DATA foobar TYPE abap_bool.`,
      js: `let foobar = new abap.types.Character(1, {"qualifiedName":"ABAP_BOOL","ddicName":"ABAP_BOOL"});`},
    {abap: `DATA foobar TYPE sy-langu.`,
      js: `let foobar = new abap.types.Character(1, {"qualifiedName":"sy-langu","conversionExit":"ISOLA"});`},
    {abap: `APPEND INITIAL LINE TO <table>.`,
      js: `fs_table_.appendInitial();`},
    {abap: `WAIT FOR PUSH CHANNELS UNTIL lo_handler->message IS NOT INITIAL UP TO 10 SECONDS.`,
      js: `if (abap.Classes['KERNEL_PUSH_CHANNELS'] === undefined) throw new Error("Wait, kernel class missing");
await abap.Classes['KERNEL_PUSH_CHANNELS'].wait({seconds: new abap.types.Integer().set(10),cond: () => {return abap.compare.initial(lo_handler.get().message) === false;}});`},
    {abap: `ADD 2 to foo.`,
      js: `foo.set(abap.operators.add(foo,new abap.types.Integer().set(2)));`},
    {abap: `ASSIGN lv_test_ref->* TO <lv_test>.`,
      js: `abap.statements.assign({target: fs_lv_test_, source: (lv_test_ref).dereference()});`},
    {abap: `LOOP AT mr_source_tree->* INTO ls_node. ENDLOOP.`,
      js: `for await (const unique1 of abap.statements.loop(mr_source_tree.dereference())) {
  ls_node.set(unique1);
}`},
    {abap: `LOOP AT foo ASSIGNING <fs>. ENDLOOP.`,
      js: `for await (const unique1 of abap.statements.loop(foo)) {
  fs_fs_.assign(unique1);
}`},
    {abap: `LOOP AT table ASSIGNING <n> USING KEY array_index WHERE path = 'hello1'. ENDLOOP.`,
      js: `for await (const unique1 of abap.statements.loop(table,{usingKey: "array_index",where: async (I) => {return abap.compare.eq(I.path, new abap.types.Character(6).set('hello1'));},topEquals: {"path": new abap.types.Character(6).set('hello1')}})) {
  fs_n_.assign(unique1);
}`},
    {abap: `LOOP AT mt_json_tree ASSIGNING <n> USING KEY (lv_tab_key) WHERE path = iv_parent_path. ENDLOOP.`,
      js: `for await (const unique1 of abap.statements.loop(mt_json_tree,{usingKey: lv_tab_key.get(),where: async (I) => {return abap.compare.eq(I.path, iv_parent_path);},topEquals: {"path": iv_parent_path}})) {
  fs_n_.assign(unique1);
}`},
    {abap: `lo_result->if_ci_test~navigate( ).`,
      js: `await lo_result.get().if_ci_test$navigate();`},
    {abap: `WRITE bar+30(*).`,
      js: `abap.statements.write(bar.getOffset({offset: 30}));`},
    {abap: `mi_merge->get_result( )-stage->count( ).`,
      js: `await (await mi_merge.get().get_result()).get().stage.get().count();`},

    {abap: `CREATE DATA dref TYPE STANDARD TABLE OF ('T000') WITH DEFAULT KEY.`,
      js: `abap.statements.createData(dref,{"name": 'T000',"table": true});`},
    {abap: `CREATE DATA dref TYPE STANDARD TABLE OF (mv_tab) WITH DEFAULT KEY.`,
      js: `abap.statements.createData(dref,{"name": mv_tab.get(),"table": true});`},
    {abap: `CREATE DATA ref LIKE LINE OF <tab>.`,
      js: `abap.statements.createData(ref,{"likeLineOf": fs_tab_});`},
    {abap: `CREATE DATA abc1 TYPE HANDLE abc2.`,
      js: `if (abap.Classes['KERNEL_CREATE_DATA_HANDLE'] === undefined) throw new Error("CreateData, kernel class missing");
await abap.Classes['KERNEL_CREATE_DATA_HANDLE'].call({handle: abc2, dref: abc1});`},

    {abap: `ASSERT <fs> IS ASSIGNED.`,
      js: `abap.statements.assert(abap.compare.assigned(fs_fs_));`},
    {abap: `AUTHORITY-CHECK OBJECT 'ZFOOBAR' ID 'ACTVT' FIELD '03'.`,
      js: `if (abap.Classes['KERNEL_AUTHORITY_CHECK'] === undefined) throw new Error("AuthorityCheck, kernel class missing");
await abap.Classes['KERNEL_AUTHORITY_CHECK'].call({});`}, // todo

    {abap: `CALL METHOD bar RECEIVING field = field.`,
      js: `field.set(await bar());`},
    {abap: `CALL METHOD bar->name( ).`,
      js: `await bar.get().name();`},
    {abap: `CALL METHOD bar->name.`,
      js: `await bar.get().name();`},

    {abap: `lo_sdescr->get_component_type(
  EXPORTING
    p_name      = rs_node_type-target_field_name
  RECEIVING
    p_descr_ref = rs_node_type-dd
  EXCEPTIONS
    component_not_found = 4 ).`,
    js: `try {
  rs_node_type.get().dd.set(await lo_sdescr.get().get_component_type({p_name: rs_node_type.get().target_field_name}));
  abap.builtin.sy.get().subrc.set(0);
} catch (e) {
  if (e.classic) {
      switch (e.classic.toUpperCase()) {
      case "COMPONENT_NOT_FOUND": abap.builtin.sy.get().subrc.set(4); break;
        }
    } else {
        throw e;
    }
  }`},

    {abap: `MODIFY ztab FROM TABLE tab.`,
      js: `await abap.statements.modifyDatabase("ztab", {"table": tab});`},
    {abap: `MODIFY (mv_table) FROM TABLE <fs>.`,
      js: `await abap.statements.modifyDatabase(mv_table.get().trimEnd().toLowerCase(), {"table": fs_fs_});`},

    {abap: `DELETE zqueue FROM TABLE lt_queue.`,
      js: `await abap.statements.deleteDatabase("zqueue", {"table": lt_queue});`},

    {abap: `DELETE ct_files
      WHERE item IS INITIAL
      AND NOT ( file-path = zif=>c_dir
      AND file-filename = zif=>c_dot ).`,
    js: `await abap.statements.deleteInternal(ct_files,{where: (I) => {return abap.compare.initial(I.item) && !(abap.compare.eq(I.file.get().path, abap.Classes['ZIF'].c_dir) && abap.compare.eq(I.file.get().filename, abap.Classes['ZIF'].c_dot));}});`},

    {abap: "lo_foo ?= lo_bar.", js: "await abap.statements.cast(lo_foo, lo_bar);", skip: false},

    {abap: "RETRY.", js: `throw new Error("Retry, not supported, transpiler");`, skip: false},

    {abap: "delete foo where instance->field_type not in types.",
      js: `await abap.statements.deleteInternal(foo,{where: (I) => {return !abap.compare.in(I.instance.get().field_type, types);}});`, skip: false},

    {abap: "READ REPORT name INTO text STATE 'A'.",
      js: `abap.statements.readReport(name, {into: text,state: new abap.types.Character(1).set('A')});`, skip: false},

    {abap: "WAIT UNTIL gv_semaphore = 'X'.",
      js: `await abap.statements.wait({cond: () => {return abap.compare.eq(gv_semaphore, new abap.types.Character(1).set('X'));}});`, skip: false},

    {abap: "MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.",
      js: `await abap.statements.message({exception: lx_exception, type: new abap.types.Character(1).set('S'), displayLike: new abap.types.Character(1).set('E')});`, skip: false},
  ];

  for (const test of tests) {
    if (test.skip) {
      it.skip(test.abap, async () => {
        return;
      });
    } else if (test.only) {
      it.only(test.abap, async () => {
        UniqueIdentifier.reset();
        const options: ITranspilerOptions = {ignoreSyntaxCheck: true, skipConstants: true};
        expect(await runSingle(test.abap, options)).to.equal(test.js);
      });
    } else {
      it(test.abap, async () => {
        UniqueIdentifier.reset();
        const options: ITranspilerOptions = {ignoreSyntaxCheck: true, skipConstants: true};
        expect(await runSingle(test.abap, options)).to.equal(test.js);
      });
    }
  }

});
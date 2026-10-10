import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

async function run(source: string, filename: string, sharedTypeFactories: boolean) {
  const abap = new ABAP({console: new MemoryConsole()});
  const code = await runFiles(abap, [{filename, contents: source}],
    {sharedTypeFactories, skipDatabaseSetup: true});
  await new AsyncFunction("abap", code)(abap);
  return abap.console.get();
}

async function expectSameModes(source: string, filename: string, expected: string) {
  const legacy = await run(source, filename, false);
  const shared = await run(source, filename, true);
  expect(shared).to.equal(expected);
  expect(shared).to.equal(legacy);
}

describe("Running Examples - Shared type factories", () => {

  it("constructs independent nested structures and table rows", async () => {
    const source = `TYPES: BEGIN OF ty_child,
  value TYPE i,
END OF ty_child.
TYPES: BEGIN OF ty_parent,
  child TYPE ty_child,
END OF ty_parent.
DATA first TYPE ty_parent.
DATA second TYPE ty_parent.
DATA first_rows TYPE STANDARD TABLE OF ty_child WITH DEFAULT KEY WITH HEADER LINE.
DATA second_rows TYPE STANDARD TABLE OF ty_child WITH DEFAULT KEY WITH HEADER LINE.
FIELD-SYMBOLS <first_row> TYPE ty_child.
FIELD-SYMBOLS <second_row> TYPE ty_child.
first-child-value = 9.
first_rows-value = 7.
APPEND INITIAL LINE TO first_rows ASSIGNING <first_row>.
<first_row>-value = 8.
APPEND INITIAL LINE TO second_rows ASSIGNING <second_row>.
WRITE / second-child-value.
WRITE / second_rows-value.
WRITE / <second_row>-value.
WRITE / lines( second_rows ).`;
    await expectSameModes(source, "zfactory.prog.abap", "0\n0\n0\n1");
  });

  it("creates independent reference wrappers for composite targets", async () => {
    const source = `TYPES: BEGIN OF ty_child,
  value TYPE i,
END OF ty_child.
DATA first TYPE REF TO ty_child.
DATA second TYPE REF TO ty_child.
CREATE DATA first.
CREATE DATA second.
first->value = 5.
WRITE second->value.`;
    await expectSameModes(source, "zfactory_ref.prog.abap", "0");
  });

  it("preserves include aliases inside each fresh factory result", async () => {
    const source = `TYPES: BEGIN OF ty_child,
  value TYPE i,
END OF ty_child.
TYPES BEGIN OF ty_parent.
INCLUDE TYPE ty_child AS named RENAMING WITH SUFFIX _s.
TYPES END OF ty_parent.
DATA first TYPE ty_parent.
DATA second TYPE ty_parent.
first-named-value = 9.
second-value_s = 12.
WRITE / first-value_s.
CLEAR first.
WRITE / second-named-value.
WRITE / second-value_s.`;
    await expectSameModes(source, "zfactory_include.prog.abap", "9\n12\n12");
  });

  it("preserves VALUE, NEW, CORRESPONDING, and field-symbol construction", async () => {
    const source = `FORM foo.
TYPES: BEGIN OF ty_value,
  value TYPE i,
END OF ty_value.
DATA first TYPE ty_value.
DATA second TYPE ty_value.
DATA source_value TYPE ty_value.
DATA copied TYPE ty_value.
DATA ref TYPE REF TO ty_value.
source_value = VALUE ty_value( value = 8 ).
copied = CORRESPONDING ty_value( source_value ).
FIELD-SYMBOLS <fs> TYPE ty_value.
ASSIGN copied TO <fs>.
<fs>-value = 12.
ref = NEW ty_value( ).
ref->value = 4.
WRITE / first-value.
WRITE / second-value.
WRITE / source_value-value.
WRITE / copied-value.
WRITE / ref->value.
DATA(inline_value) = VALUE ty_value( value = 16 ).
WRITE / inline_value-value.
ENDFORM.
START-OF-SELECTION.
PERFORM foo.`;
    await expectSameModes(source, "zfactory_expr.prog.abap", "0\n0\n8\n12\n4\n16");
  });

  it("preserves class attributes and optional returning parameters", async () => {
    const source = `TYPES: BEGIN OF ty_value,
  value TYPE i,
END OF ty_value.
INTERFACE lif.
ENDINTERFACE.
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    DATA attribute TYPE ty_value.
    METHODS copy IMPORTING input TYPE ty_value OPTIONAL
      RETURNING VALUE(output) TYPE ty_value.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD copy.
    output = input.
  ENDMETHOD.
ENDCLASS.
DATA object TYPE REF TO lcl.
CREATE OBJECT object.
DATA interface_object TYPE REF TO lif.
interface_object = object.
object = CAST #( interface_object ).
DATA first TYPE ty_value.
DATA result TYPE ty_value.
DATA missing TYPE ty_value.
first-value = 7.
object->attribute-value = 4.
result = object->copy( input = first ).
missing = object->copy( ).
WRITE / object->attribute-value.
WRITE / result-value.
WRITE / missing-value.`;
    await expectSameModes(source, "zfactory_class.prog.abap", "4\n7\n0");
  });

});

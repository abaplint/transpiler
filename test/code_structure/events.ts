import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running code structure - Eventing", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
    METHODS raise.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD raise.
    RAISE EVENT foo.
  ENDMETHOD.
ENDCLASS.

CLASS handler DEFINITION.
  PUBLIC SECTION.
    METHODS bar FOR EVENT foo OF lcl.
ENDCLASS.
CLASS handler IMPLEMENTATION.
  METHOD bar.
    WRITE 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  DATA hnd TYPE REF TO handler.
  CREATE OBJECT ref.
  CREATE OBJECT hnd.
  SET HANDLER hnd->bar FOR ref.
  ref->raise( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("call event handler method", async () => {
    const code = `
INTERFACE lif.
  EVENTS foo EXPORTING VALUE(action) TYPE string.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS on_event FOR EVENT foo OF lif IMPORTING action.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD on_event.
    WRITE action.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->on_event( 'sdf' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("sdf");
  });

  it("basic deregistration", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    EVENTS foo.
    METHODS raise.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD raise.
    RAISE EVENT foo.
  ENDMETHOD.
ENDCLASS.

CLASS handler DEFINITION.
  PUBLIC SECTION.
    METHODS bar FOR EVENT foo OF lcl.
ENDCLASS.
CLASS handler IMPLEMENTATION.
  METHOD bar.
    WRITE 'handled'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  DATA hnd TYPE REF TO handler.
  CREATE OBJECT ref.
  CREATE OBJECT hnd.
  SET HANDLER hnd->bar FOR ref.
  ref->raise( ).
  SET HANDLER hnd->bar FOR ref ACTIVATION abap_false.
  ref->raise( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("handled");
  });

  it("private handler method", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run.
    EVENTS foo.
  PRIVATE SECTION.
    METHODS handler FOR EVENT foo OF lcl.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    SET HANDLER handler FOR me.
    RAISE EVENT foo.
  ENDMETHOD.

  METHOD handler.
    WRITE / 'hello'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl.
  CREATE OBJECT obj.
  obj->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("private handler method", async () => {
    const code = `
CLASS lclevent DEFINITION.
  PUBLIC SECTION.
    EVENTS link_click.
ENDCLASS.
CLASS lclevent IMPLEMENTATION.
ENDCLASS.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    CLASS-METHODS on_link_click FOR EVENT link_click OF lclevent.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA lo_event TYPE REF TO lclevent.
    SET HANDLER on_link_click FOR lo_event.
  ENDMETHOD.
  METHOD on_link_click.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("handlers are registered for the specified sender", async () => {
    const code = `
CLASS lcl_sender DEFINITION.
  PUBLIC SECTION.
    EVENTS fired.
    METHODS fire.
ENDCLASS.

CLASS lcl_sender IMPLEMENTATION.
  METHOD fire.
    RAISE EVENT fired.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    DATA hits TYPE i READ-ONLY.
    METHODS constructor IMPORTING sender TYPE REF TO lcl_sender.
    METHODS on_fired FOR EVENT fired OF lcl_sender.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD constructor.
    SET HANDLER on_fired FOR sender.
  ENDMETHOD.

  METHOD on_fired.
    hits = hits + 1.
  ENDMETHOD.
ENDCLASS.

DATA sender_1  TYPE REF TO lcl_sender.
DATA sender_2  TYPE REF TO lcl_sender.
DATA handler_1 TYPE REF TO lcl_handler.
DATA handler_2 TYPE REF TO lcl_handler.

START-OF-SELECTION.
  sender_1  = NEW lcl_sender( ).
  sender_2  = NEW lcl_sender( ).
  handler_1 = NEW lcl_handler( sender_1 ).
  handler_2 = NEW lcl_handler( sender_2 ).

  sender_1->fire( ).

  ASSERT handler_1->hits = 1.
  ASSERT handler_2->hits = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("deregister handler for one receiver only", async () => {
    const code = `
CLASS lcl_sender DEFINITION FINAL.
  PUBLIC SECTION.
    EVENTS ping.
    METHODS fire.
ENDCLASS.

CLASS lcl_receiver DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA result TYPE string.

    METHODS constructor
      IMPORTING
        iv_name TYPE c.

    METHODS on_ping
      FOR EVENT ping OF lcl_sender.

    METHODS register
      IMPORTING
        io_sender TYPE REF TO lcl_sender.

    METHODS unregister
      IMPORTING
        io_sender TYPE REF TO lcl_sender.

  PRIVATE SECTION.
    DATA name TYPE c LENGTH 1.
ENDCLASS.

CLASS lcl_sender IMPLEMENTATION.
  METHOD fire.
    RAISE EVENT ping.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_receiver IMPLEMENTATION.
  METHOD constructor.
    name = iv_name.
  ENDMETHOD.

  METHOD on_ping.
    result = result && name.
  ENDMETHOD.

  METHOD register.
    SET HANDLER me->on_ping FOR io_sender.
  ENDMETHOD.

  METHOD unregister.
    SET HANDLER me->on_ping FOR io_sender ACTIVATION abap_false.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA sender TYPE REF TO lcl_sender.
  DATA receiver_a TYPE REF TO lcl_receiver.
  DATA receiver_b TYPE REF TO lcl_receiver.

  sender = NEW lcl_sender( ).
  receiver_a = NEW lcl_receiver( iv_name = 'A' ).
  receiver_b = NEW lcl_receiver( iv_name = 'B' ).

  receiver_a->register( sender ).
  receiver_b->register( sender ).

  " Must deactivate receiver B only.
  receiver_b->unregister( sender ).

  sender->fire( ).

  WRITE / |Expected A, actual { lcl_receiver=>result }|.
  ASSERT lcl_receiver=>result = 'A'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("subrc", async () => {
    const code = `
CLASS lcl_sender DEFINITION FINAL.
  PUBLIC SECTION.
    EVENTS ping.
ENDCLASS.

CLASS lcl_receiver DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS on_ping FOR EVENT ping OF lcl_sender.
ENDCLASS.

CLASS lcl_sender IMPLEMENTATION.
ENDCLASS.

CLASS lcl_receiver IMPLEMENTATION.
  METHOD on_ping.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA sender   TYPE REF TO lcl_sender.
  DATA receiver TYPE REF TO lcl_receiver.
  DATA empty    TYPE STANDARD TABLE OF i WITH EMPTY KEY.

  CREATE OBJECT sender.
  CREATE OBJECT receiver.

  SET HANDLER receiver->on_ping FOR sender.

  " Seed a nonzero return code. The following successful SET HANDLER
  " must replace it with zero.
  READ TABLE empty INDEX 1 TRANSPORTING NO FIELDS.
  ASSERT sy-subrc = 4.

  SET HANDLER receiver->on_ping FOR sender ACTIVATION abap_false.

  WRITE: / |Expected sy-subrc 0, actual { sy-subrc }|.
  ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});

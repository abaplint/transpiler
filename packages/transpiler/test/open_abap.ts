import {expect} from "chai";
import {Transpiler} from "../src";

describe("open abap examples", () => {

  it("intf + clas", async () => {
    const intf = `
    INTERFACE zif_abap_serverless_v1 PUBLIC.

    TYPES: BEGIN OF ty_header,
             field TYPE string,
             value TYPE string,
           END OF ty_header.

    TYPES ty_headers TYPE STANDARD TABLE OF ty_header WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_http,
             headers TYPE ty_headers,
             body    TYPE string,
           END OF ty_http.

    METHODS
      run
        IMPORTING
          method      TYPE string OPTIONAL
          path        TYPE string OPTIONAL
          query       TYPE string OPTIONAL
          request     TYPE ty_http OPTIONAL
        RETURNING
          VALUE(response) TYPE ty_http
        RAISING
          cx_static_check.

  ENDINTERFACE.`;

    const clas = `
CLASS zcl_words DEFINITION PUBLIC FINAL CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES zif_abap_serverless_v1.
ENDCLASS.

CLASS ZCL_WORDS IMPLEMENTATION.
  METHOD zif_abap_serverless_v1~run.
    response-body = 'moo'.
  ENDMETHOD.
ENDCLASS.`;

    const files = [{filename: "zif_abap_serverless_v1.intf.abap", contents: intf},
      {filename: "zcl_words.clas.abap", contents: clas}];

    const output = await new Transpiler().run(files);

    expect(output[0].js.contents).to.contain("moo");
  });

});

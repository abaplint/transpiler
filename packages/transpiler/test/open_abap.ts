import {expect} from "chai";
import {IFile, Transpiler} from "../src";
import * as abaplint from "@abaplint/core";

async function runFiles(files: IFile[]) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
  const res = await new Transpiler().run(reg);
  return res.objects;
}

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

    const files = [
      {filename: "zif_abap_serverless_v1.intf.abap", contents: intf},
      {filename: "zcl_words.clas.abap", contents: clas}];

    const output = await runFiles(files);

    expect(output[1].chunk.getCode()).to.contain("moo");
  });

});

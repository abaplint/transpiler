import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {Transpiler} from "../src";
import {CDSMetadata, ICDSMetadata} from "../src/cds_metadata";
import {t000} from "./_cds";

const travel = `@AbapCatalog.sqlViewName: 'ZVTRAVEL'
@EndUserText.label: 'Travel'
@UI.headerInfo: { typeName: 'Travel', title.value: 'TravelId' }
define root view ZI_TRAVEL as select from t000
  composition [0..*] of ZI_BOOKING as _Booking
  association [0..1] to t001 as _Agency on $projection.TravelId = _Agency.mandt
  association to one t002 as _Text on $projection.TravelId = _Text.mandt with default filter _Text.cccategory = 'E'
{
  @UI.lineItem: [{ position: 10, importance: #HIGH }]
  @Search.defaultSearchElement
  key mandt as TravelId,
  @ObjectModel.text.element: ['Category']
  @UI.hidden: false
  @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_X', element: 'Id' } }]
  cccategory as Category,
  _Booking,
  @Consumption.filter.hidden: true
  _Agency
}`;

const booking = `define view entity ZI_BOOKING as select from t001
  association to parent ZI_TRAVEL as _Travel on $projection.TravelId = _Travel.TravelId
  association of many to exact one t002 as _Exact on $projection.TravelId = _Exact.mandt
  association [1..*] to t002 as _Many on $projection.TravelId = _Many.mandt
{
  key mandt as TravelId,
  key cccategory as BookingId,
  _Travel
}`;

const projection = `define root view entity ZC_TRAVEL provider contract transactional_query as projection on ZI_TRAVEL
{
  key TravelId,
  Category,
  _Booking : redirected to composition child ZC_BOOKING,
  _Agency
}`;

const abstract = `@EndUserText.label: 'Parameter'
define abstract entity ZA_PARAM {
  @EndUserText.label: 'Days'
  key days : abap.int4;
  note : abap.char(20);
  _Travel : association to ZI_TRAVEL on _Travel.TravelId = $projection.days;
}`;

const node = `define view entity ZI_NODE
  as select from t000
  association [0..1] to ZI_NODE as _Parent on $projection.ParentId = _Parent.NodeId
{
  key mandt      as NodeId,
      cccategory as ParentId,
      _Parent
}`;

const hierarchy = `define hierarchy ZI_NODE_HIERARCHY
  as parent child hierarchy(
    source ZI_NODE
    child to parent association _Parent
    start where ParentId is initial
    siblings order by NodeId
  )
{
  NodeId,
  ParentId
}`;

const hierarchyAlias = `@EndUserText.label: 'Nodes'
define hierarchy ZI_NODE_HIERARCHY_ALIAS
  as parent child hierarchy(
    source ZI_NODE
    child to parent association _Parent
    start where ParentId is initial
    siblings order by NodeId
  )
{
  key NodeId as Id,
  @EndUserText.label: 'Parent'
  ParentId,
  _Parent
}`;

function registry(): abaplint.IRegistry {
  const files = [
    ...["t000", "t001", "t002"].map(n => ({filename: n + ".tabl.xml", contents: t000.replace(/T000/g, n.toUpperCase())})),
    {filename: "zi_travel.ddls.asddls", contents: travel},
    {filename: "zi_booking.ddls.asddls", contents: booking},
    {filename: "zc_travel.ddls.asddls", contents: projection},
    {filename: "za_param.ddls.asddls", contents: abstract},
    {filename: "zi_node.ddls.asddls", contents: node},
    {filename: "zi_node_hierarchy.ddls.asddls", contents: hierarchy},
    {filename: "zi_node_hierarchy_alias.ddls.asddls", contents: hierarchyAlias},
  ];
  return new abaplint.Registry().addFiles(files.map(f => new abaplint.MemoryFile(f.filename, f.contents))).parse();
}

function metadata(name: string): ICDSMetadata {
  const reg = registry();
  const result = new CDSMetadata(reg).build(reg.getObject("DDLS", name) as abaplint.Objects.DataDefinition);
  expect(result).to.not.equal(undefined);
  return result!;
}

describe("CDS metadata", () => {

  it("entity annotations and definition type", () => {
    const m = metadata("ZI_TRAVEL");
    expect(m.definitionType).to.equal("VIEW");
    expect(m.root).to.equal(true);
    expect(m.annotations).to.deep.equal([
      {name: "AbapCatalog.sqlViewName", value: "ZVTRAVEL"},
      {name: "EndUserText.label", value: "Travel"},
      {name: "UI.headerInfo", value: {typeName: "Travel", title: {value: "TravelId"}}},
    ]);
    expect(metadata("ZI_BOOKING").definitionType).to.equal("VIEW_ENTITY");
  });

  it("fields with keys and annotation values", () => {
    const m = metadata("ZI_TRAVEL");
    expect(m.fields.map(f => f.name)).to.deep.equal(["TravelId", "Category"]);
    expect(m.fields[0].key).to.equal(true);
    expect(m.fields[1].key).to.equal(false);
    expect(m.fields[0].annotations).to.deep.equal([
      {name: "UI.lineItem", value: [{position: 10, importance: {"#": "HIGH"}}]},
      {name: "Search.defaultSearchElement", value: true},
    ]);
    expect(m.fields[1].annotations).to.deep.equal([
      {name: "ObjectModel.text.element", value: ["Category"]},
      {name: "UI.hidden", value: false},
      {name: "Consumption.valueHelpDefinition", value: [{entity: {name: "ZI_X", element: "Id"}}]},
    ]);
  });

  it("associations and compositions", () => {
    const m = metadata("ZI_TRAVEL");
    expect(m.associations).to.deep.equal([
      {name: "_Agency", target: "t001", type: "ASSOCIATION", cardinality: {min: 0, max: 1},
        condition: "$projection.TravelId = _Agency.mandt", defaultFilter: undefined, exposed: true,
        annotations: [{name: "Consumption.filter.hidden", value: true}]},
      {name: "_Text", target: "t002", type: "ASSOCIATION", cardinality: {min: 0, max: 1},
        condition: "$projection.TravelId = _Text.mandt", defaultFilter: "_Text.cccategory = 'E'", exposed: false,
        annotations: []},
      {name: "_Booking", target: "ZI_BOOKING", type: "COMPOSITION", cardinality: {min: 0, max: "*"},
        condition: undefined, defaultFilter: undefined, exposed: true, annotations: []},
    ]);
  });

  it("cardinalities and to parent", () => {
    const m = metadata("ZI_BOOKING");
    expect(m.associations.map(a => [a.name, a.type, a.cardinality])).to.deep.equal([
      ["_Travel", "TO_PARENT", {min: 1, max: 1}],
      ["_Exact", "ASSOCIATION", {min: 1, max: 1}],
      ["_Many", "ASSOCIATION", {min: 1, max: "*"}],
    ]);
  });

  it("projection, redirected and inherited associations", () => {
    const m = metadata("ZC_TRAVEL");
    expect(m.definitionType).to.equal("PROJECTION_VIEW");
    expect(m.fields.map(f => f.name)).to.deep.equal(["TravelId", "Category"]);
    expect(m.associations.map(a => [a.name, a.target, a.type, a.cardinality, a.condition])).to.deep.equal([
      ["_Booking", "ZC_BOOKING", "COMPOSITION", {min: 0, max: "*"}, undefined],
      ["_Agency", "t001", "ASSOCIATION", {min: 0, max: 1}, "$projection.TravelId = _Agency.mandt"],
    ]);
  });

  it("abstract entity", () => {
    const m = metadata("ZA_PARAM");
    expect(m.definitionType).to.equal("ABSTRACT_ENTITY");
    expect(m.fields).to.deep.equal([
      {name: "days", key: true, virtual: false, annotations: [{name: "EndUserText.label", value: "Days"}]},
      {name: "note", key: false, virtual: false, annotations: []},
    ]);
    expect(m.associations.map(a => [a.name, a.target, a.exposed])).to.deep.equal([["_Travel", "ZI_TRAVEL", true]]);
  });

  it("hierarchy elements", () => {
    const m = metadata("ZI_NODE_HIERARCHY");
    expect(m.definitionType).to.equal("HIERARCHY");
    expect(m.fields).to.deep.equal([
      {name: "NodeId", key: false, virtual: false, annotations: []},
      {name: "ParentId", key: false, virtual: false, annotations: []},
    ]);
    expect(m.associations).to.deep.equal([]);
  });

  it("hierarchy with key, alias, annotations and an exposed source association", () => {
    const m = metadata("ZI_NODE_HIERARCHY_ALIAS");
    expect(m.definitionType).to.equal("HIERARCHY");
    expect(m.annotations).to.deep.equal([{name: "EndUserText.label", value: "Nodes"}]);
    expect(m.fields).to.deep.equal([
      {name: "Id", key: true, virtual: false, annotations: []},
      {name: "ParentId", key: false, virtual: false, annotations: [{name: "EndUserText.label", value: "Parent"}]},
    ]);
    expect(m.associations).to.deep.equal([
      {name: "_Parent", target: "ZI_NODE", type: "ASSOCIATION", cardinality: {min: 0, max: 1},
        condition: "$projection.ParentId = _Parent.NodeId", defaultFilter: undefined, exposed: true, annotations: []},
    ]);
  });

  it("enum expressions, numbers and null", () => {
    const reg = new abaplint.Registry().addFile(new abaplint.MemoryFile("zddls.ddls.asddls",
      `@ObjectModel.usageType.sizeCategory: #XL
@Metadata.ignorePropagatedAnnotations: true
@Analytics.dataExtraction.delta.byElement.maxDelayInSeconds: 1800
@UI.presentationVariant: [{ maxItems: null }]
define abstract entity ZDDLS { key mandt : abap.clnt; }`)).parse();
    const m = new CDSMetadata(reg).build(reg.getObject("DDLS", "ZDDLS") as abaplint.Objects.DataDefinition)!;
    expect(m.annotations).to.deep.equal([
      {name: "ObjectModel.usageType.sizeCategory", value: {"#": "XL"}},
      {name: "Metadata.ignorePropagatedAnnotations", value: true},
      {name: "Analytics.dataExtraction.delta.byElement.maxDelayInSeconds", value: 1800},
      {name: "UI.presentationVariant", value: [{maxItems: null}]},
    ]);
  });

});

describe("DDLS output", () => {

  it("registers the row type and the metadata in abap.DDIC", async () => {
    const res = await new Transpiler().run(registry());
    const ddls = res.objects.find(o => o.object.type === "DDLS" && o.object.name === "ZI_TRAVEL");
    expect(ddls?.filename).to.equal("zi_travel.ddls.mjs");
    const code = ddls!.chunk.getCode();
    expect(code).to.include(`abap.DDIC["ZI_TRAVEL"] = {`);
    expect(code).to.include(`"objectType": "DDLS"`);
    expect(code).to.include(`"keyFields": ["TRAVELID"]`);
    expect(code).to.include(`"sqlViewName": "ZVTRAVEL"`);
    expect(code).to.include(`"cds": {"definitionType":"VIEW"`);
    // exposed associations are not components of the row type
    expect(code).to.include(`"travelid"`);
    expect(code).to.not.include(`"_booking"`);
    expect(code).to.not.include(`"_agency"`);
    expect(res.initializationScript).to.include("zi_travel.ddls.mjs");
  });

  it("creates the database view of a CDS view with associations", async () => {
    const res = await new Transpiler().run(registry());
    expect(res.databaseSetup.schemas.sqlite).to.include(
      `CREATE VIEW "zi_travel" AS SELECT "t000"."mandt" AS "travelid", "t000"."cccategory" AS "category" FROM "t000";`);
    expect(res.databaseSetup.schemas.sqlite).to.include(
      `CREATE VIEW "zi_booking" AS SELECT "t001"."mandt" AS "travelid", "t001"."cccategory" AS "bookingid" FROM "t001";`);
  });

  it("merges into the folder DDIC module in grouped output", async () => {
    const reg = registry();
    const folders = new Map<string, string>([...reg.getObjects()].flatMap(o => o.getFiles().map(f => [f.getFilename(), "src"] as [string, string])));
    const res = await new Transpiler().run(reg, undefined, folders);
    const ddic = res.objects.find(o => o.filename === "src/_ddic.mjs");
    expect(ddic?.chunk.getCode()).to.include(`abap.DDIC["ZI_TRAVEL"]`);
    expect(res.objects.some(o => o.object.type === "DDLS")).to.equal(false);
  });

});

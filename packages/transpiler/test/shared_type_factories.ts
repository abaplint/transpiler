import {expect} from "chai";
import {Transpiler} from "../src";
import {IFile, ITranspilerOptions} from "../src/types";
import * as abaplint from "@abaplint/core";
import * as sourceMap from "source-map";
import {runSingleMapped, validateSourceMap} from "./_utils";
import {Chunk} from "../src/chunk";
import {TypeFactoryRegistry} from "../src/type_factory_registry";
import {TranspileTypes} from "../src/transpile_types";

const source = `TYPES: BEGIN OF ty_child,
  value TYPE i,
END OF ty_child.
TYPES: BEGIN OF ty_parent,
  left TYPE ty_child,
  right TYPE ty_child,
END OF ty_parent.
DATA first TYPE ty_parent.
DATA second TYPE ty_parent.
DATA first_rows TYPE STANDARD TABLE OF ty_child WITH DEFAULT KEY.
DATA second_rows TYPE STANDARD TABLE OF ty_child WITH DEFAULT KEY.`;

async function transpileFiles(files: IFile[], options?: ITranspilerOptions) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  return new Transpiler(options).run(new abaplint.Registry().addFiles(memory));
}

async function transpile(contents: string, options?: ITranspilerOptions) {
  const output = await transpileFiles([{filename: "zfactory.prog.abap", contents}], options);
  return output.objects[0]?.chunk.getCode();
}

describe("shared type factories", () => {

  it("deduplicates nested structures and table row constructors per module", async () => {
    const code = await transpile(source, {sharedTypeFactories: true});
    expect(code).to.be.a("string");
    expect(code!.match(/function \$t_/g)?.length).to.equal(3);
    expect(code).to.include("let first = $t_");
    expect(code).to.include("let second = $t_");
    expect(code).to.include("let first_rows = $t_");
    expect(code).to.include("let second_rows = $t_");
  });

  it("keeps the legacy inline output when disabled", async () => {
    const code = await transpile(source, {sharedTypeFactories: false});
    expect(code).to.include("new abap.types.Structure({");
    expect(code).not.to.include("$t_");
  });

  it("enables shared factories by default", async () => {
    const code = await transpile(source);
    expect(code).to.include("function $t_");
    expect(code).to.include("let first = $t_");
  });

  it("emits factories inside the raw module chunk without helper imports", async () => {
    const result = await transpileFiles([{filename: "zraw.prog.abap", contents: source}], {sharedTypeFactories: true});
    expect(result.objects).to.have.length(1);
    const code = result.objects[0].chunk.getCode();
    expect(code).to.include("function $t_");
    expect(code).not.to.match(/(?:import|export)\s*\(/);
  });

  it("keeps constructors distinct when field order or elementary metadata differs", async () => {
    const input = `TYPES: BEGIN OF ty_first,
  left TYPE c LENGTH 8,
  right TYPE i,
END OF ty_first.
TYPES: BEGIN OF ty_second,
  right TYPE i,
  left TYPE c LENGTH 10,
END OF ty_second.
DATA first TYPE ty_first.
DATA first_copy TYPE ty_first.
DATA second TYPE ty_second.`;
    const code = await transpile(input + "\nDATA second_copy TYPE ty_second.", {sharedTypeFactories: true});
    expect(code!.match(/function \$t_/g)?.length).to.equal(2);
  });

  it("keeps table constructors distinct when table options differ", async () => {
    const input = `TYPES: BEGIN OF ty_item,
  key TYPE c LENGTH 20,
  value TYPE string,
END OF ty_item.
TYPES ty_std TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY.
TYPES ty_sorted TYPE SORTED TABLE OF ty_item WITH UNIQUE KEY key.
DATA std_1 TYPE ty_std.
DATA std_2 TYPE ty_std.
DATA sorted_1 TYPE ty_sorted.
DATA sorted_2 TYPE ty_sorted.`;
    const code = await transpile(input, {sharedTypeFactories: true});
    expect(code!.match(/function \$t_/g)?.length).to.equal(3);
    expect(code).to.include('"type":"STANDARD"');
    expect(code).to.include('"type":"SORTED"');
  });

  it("keeps composite reference factories distinct by target metadata", async () => {
    const input = `TYPES: BEGIN OF ty_item,
  key TYPE c LENGTH 20,
  value TYPE string,
END OF ty_item.
TYPES: BEGIN OF ty_other,
  key TYPE c LENGTH 20,
  value TYPE string,
END OF ty_other.
DATA item_ref_1 TYPE REF TO ty_item.
DATA item_ref_2 TYPE REF TO ty_item.
DATA other_ref_1 TYPE REF TO ty_other.
DATA other_ref_2 TYPE REF TO ty_other.`;
    const code = await transpile(input, {sharedTypeFactories: true});
    expect(code!.match(/function \$t_/g)?.length).to.equal(2);
    expect(code).to.include('"TY_ITEM-VALUE"');
    expect(code).to.include('"TY_OTHER-VALUE"');
  });

  it("keeps small repeated types inline when helper code would increase output", async () => {
    const input = "TYPES: BEGIN OF ty,\n  value TYPE i,\nEND OF ty.\nDATA first TYPE ty.\nDATA second TYPE ty.";
    const legacy = await transpile(input, {sharedTypeFactories: false});
    const shared = await transpile(input, {sharedTypeFactories: true});
    expect(Buffer.byteLength(shared!, "utf8")).to.be.at.most(Buffer.byteLength(legacy!, "utf8"));
  });

  it("keeps statement source maps valid after adding helper declarations", async () => {
    const input = `TYPES: BEGIN OF ty,
  value TYPE i,
END OF ty.
DATA first TYPE ty.
DATA second TYPE ty.
WRITE first-value.`;
    const result = await runSingleMapped(input, {sharedTypeFactories: true, addCommonJS: true});
    expect(result).to.not.equal(undefined);
    expect(result!.js).to.include("function $t_");
    const stats = await validateSourceMap(input, result!.js, result!.map);
    expect(stats.mappedLines).to.be.greaterThan(0);
    expect(stats.codeLines).to.be.greaterThan(stats.mappedLines);
    const ignored = await runSingleMapped(input, {sharedTypeFactories: true, ignoreSourceMap: true});
    expect(JSON.parse(ignored!.map).mappings).to.equal("");
  });

  it("produces deterministic output across independent runs", async () => {
    const options = {sharedTypeFactories: true};
    const first = await transpile(source, options);
    const second = await transpile(source, options);
    expect(second).to.equal(first);
  });

  it("deduplicates equivalent composite type objects by emitted semantics", () => {
    const components = () => Array.from({length: 12}, (_, i) => ({
      name: "field_" + i,
      type: new abaplint.BasicTypes.CharacterType(40),
    }));
    const firstType = new abaplint.BasicTypes.StructureType(components(), "same_type");
    const secondType = new abaplint.BasicTypes.StructureType(components(), "same_type");
    const registry = new TypeFactoryRegistry("zequivalent.prog.mjs");
    const firstCall = TranspileTypes.toType(firstType, undefined, registry);
    const secondCall = TranspileTypes.toType(secondType, undefined, registry);
    const body = new Chunk(firstCall + "\n" + secondCall);
    const helpers = registry.finalize(body);
    expect(secondCall).to.equal(firstCall);
    expect(helpers.match(/function \$t_/g)?.length).to.equal(1);
  });

  it("memoizes packed types separately when rendering options change", () => {
    const type = new abaplint.BasicTypes.PackedType(8, 2);
    const registry = new TypeFactoryRegistry("zpacked.prog.mjs");
    const decimalsZero = TranspileTypes.toType(type, {packedDecimals: 0}, registry);
    const decimalsFour = TranspileTypes.toType(type, {packedDecimals: 4}, registry);
    expect(decimalsZero).to.include("decimals: 0");
    expect(decimalsFour).to.include("decimals: 4");
    expect(decimalsZero).not.to.equal(decimalsFour);
  });

  it("reports recursive resolution without overflowing", () => {
    const type = new abaplint.BasicTypes.CharacterType(1);
    const registry = new TypeFactoryRegistry("zrecursive.prog.mjs");
    expect(() => registry.resolveType(type, "{}", () => registry.resolveType(type, "{}", () => "")))
      .to.throw("Recursive ABAP type construction is not supported");
  });

  it("keeps names distinct when separate modules are concatenated", () => {
    const fields = Array.from({length: 20}, (_, i) => `"field_${i}": new abap.types.Character(40)`).join(",");
    const expression = `new abap.types.Structure({${fields}}, undefined, undefined, {}, {})`;
    const firstRegistry = new TypeFactoryRegistry("zfirst.prog.mjs");
    const firstCall = firstRegistry.register(expression);
    const firstBody = new Chunk(`${firstCall}\n${firstCall}`);
    const first = firstRegistry.finalize(firstBody) + firstBody.getCode();
    const secondRegistry = new TypeFactoryRegistry("zsecond.prog.mjs");
    const secondCall = secondRegistry.register(expression);
    const secondBody = new Chunk(`${secondCall}\n${secondCall}`);
    const second = secondRegistry.finalize(secondBody) + secondBody.getCode();
    const names = [...(first + second).matchAll(/function (\$t_[\w$]+)\(/g)].map(match => match[1]);
    expect(names).to.have.length(2);
    expect(new Set(names).size).to.equal(2);
  });

  it("shares factories across merged class-local definition and implementation files", async () => {
    const fields = Array.from({length: 12}, (_, i) => `field_${i} TYPE c LENGTH 40`).join(",\n");
    const reservedName = new TypeFactoryRegistry("zcl_factory.clas.locals.mjs")
      .register("new abap.types.Structure({\"field\": new abap.types.Character(40)}, undefined, undefined, {}, {})")
      .slice(0, -2);
    const files: IFile[] = [
      {
        filename: "zif_factory_types.intf.abap",
        contents: `INTERFACE zif_factory_types PUBLIC.
TYPES: BEGIN OF ty,
${fields},
END OF ty.
ENDINTERFACE.`,
      },
      {
        filename: "zcl_factory.clas.abap",
        contents: `CLASS zcl_factory DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-DATA item TYPE zif_factory_types=>ty.
    CLASS-METHODS get RETURNING VALUE(result) TYPE zif_factory_types=>ty.
ENDCLASS.
CLASS zcl_factory IMPLEMENTATION.
  METHOD get.
    DATA local TYPE zif_factory_types=>ty.
    result = local.
  ENDMETHOD.
ENDCLASS.`,
      },
      {
        filename: "zcl_factory.clas.locals_def.abap",
        contents: `CLASS lcl_factory DEFINITION.
  PUBLIC SECTION.
    DATA item TYPE zif_factory_types=>ty.
    METHODS get RETURNING VALUE(result) TYPE zif_factory_types=>ty.
ENDCLASS.`,
      },
      {
        filename: "zcl_factory.clas.locals_imp.abap",
        contents: `* ${reservedName}
CLASS lcl_factory IMPLEMENTATION.
  METHOD get.
    DATA local_a TYPE zif_factory_types=>ty.
    DATA local_b TYPE zif_factory_types=>ty.
    result = local_a.
  ENDMETHOD.
ENDCLASS.`,
      },
    ];
    const result = await transpileFiles(files, {sharedTypeFactories: true});
    const locals = result.objects.find(output => output.filename.endsWith(".clas.locals.mjs"));
    const main = result.objects.find(output => output.filename.endsWith(".clas.mjs"));
    expect(locals).to.not.equal(undefined);
    expect(main).to.not.equal(undefined);
    expect(locals!.chunk.getCode().match(/function \$t_/g)?.length).to.equal(1);
    expect(locals!.chunk.getCode()).not.to.include(`function ${reservedName}(`);
    expect(locals!.chunk.getCode().match(/\$t_[\w$]+\(\)/g)?.length).to.be.greaterThan(2);
    const localsMap = JSON.parse(locals!.chunk.getMap(locals!.filename));
    const consumer = await new sourceMap.SourceMapConsumer(localsMap);
    const mappedFiles = new Set<string>();
    consumer.eachMapping(mapping => {
      if (mapping.source === null || mapping.originalLine === null) {
        return;
      }
      const sourceFile = files.find(file => mapping.source!.endsWith(file.filename));
      expect(sourceFile).to.not.equal(undefined);
      expect(mapping.originalLine).to.be.at.most(sourceFile!.contents.split("\n").length);
      mappedFiles.add(sourceFile!.filename);
    });
    expect(mappedFiles.has("zcl_factory.clas.locals_imp.abap")).to.equal(true);
    const mainCode = main!.chunk.getCode();
    expect(mainCode).to.include("function $t_");
    const helperPosition = mainCode.indexOf("function $t_");
    const staticInitializerPosition = mainCode.indexOf("zcl_factory.item = $t_");
    expect(staticInitializerPosition).to.be.greaterThan(helperPosition);
  });

});

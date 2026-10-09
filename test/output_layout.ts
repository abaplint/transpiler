import {expect} from "chai";
import {execFileSync} from "child_process";
import {existsSync, mkdirSync, mkdtempSync, readFileSync, realpathSync, rmSync, symlinkSync, writeFileSync} from "fs";
import {tmpdir} from "os";
import * as path from "path";
import {libraryNames, libraryRegistry} from "../packages/cli/src/libraries";

describe("Library output names", () => {
  it("derives stable names or uses an explicit override", () => {
    expect(libraryNames([
      {url: "https://github.com/org/core.git/"},
      {url: "git@example.com:org/utils.git"},
      {url: "ssh://git@example.com/org/other.git"},
      {folder: "./deps/local/"},
      {folder: "deps\\windows"},
      {name: "custom", url: "https://example.com/other"},
    ])).to.deep.equal(["core", "utils", "other", "local", "windows", "custom"]);
  });

  it("rejects invalid, reserved, and duplicate names", () => {
    for (const name of ["", ".", "..", "../escape", "x/y", "x\\y", "NUL", "COM1.txt", "trailing.", "trailing ", "x?", "x\u0000"]) {
      expect(() => libraryNames([{name}]), name).to.throw("Invalid library name");
    }
    expect(() => libraryNames([{name: "PROJECT"}])).to.throw("reserved");
    expect(() => libraryNames([{name: "Core"}, {name: "core"}])).to.throw("Duplicate");
  });

  it("accepts generic abapGit package metadata shared by libraries", () => {
    const files = [{filename: "package.devc.xml", contents: "<abapGit/>"}, {filename: "README.md", contents: "readme"}];
    const {reg, folders} = libraryRegistry([], [{name: "one", files}, {name: "two", files}]);
    expect([...reg.getObjects()].length).to.equal(1);
    expect(folders.get("package.devc.xml")).to.equal("two");
  });

  it("preserves project precedence and rejects objects shared by two libraries", () => {
    const file = {filename: "zcl_app.clas.abap", contents: ""};
    const lib = {name: "lib", files: [file]};
    const {reg, folders} = libraryRegistry([file], [lib]);
    expect(reg.isDependency(reg.getObject("CLAS", "ZCL_APP")!)).to.equal(false);
    expect(folders.get(file.filename)).to.equal("project");
    expect(() => libraryRegistry([], [lib, {...lib, name: "other"}])).to.throw("Ambiguous dependency object");
  });
});

describe("CLI grouped output", () => {
  let folder: string;
  const cli = path.resolve("packages/cli/build/bundle.js");
  const write = (filename: string, contents: string | Buffer) => {
    const target = path.join(folder, filename);
    mkdirSync(path.dirname(target), {recursive: true});
    writeFileSync(target, contents);
  };
  const config = (libs: object[] = [], maps = true) => ({
    input_folder: ["src", "extra"],
    output_folder: "nested/output",
    libs,
    write_source_map: maps,
    write_unit_tests: true,
    options: {addCommonJS: true, setup: undefined as {filename: string, postFunction: string} | undefined},
  });
  const build = (settings: object) => {
    write("abap_transpile.json", JSON.stringify(settings));
    return execFileSync(process.execPath, [cli], {cwd: folder, encoding: "utf8", timeout: 30000});
  };
  const run = (module: string) => execFileSync(process.execPath, [module], {cwd: folder, encoding: "utf8", timeout: 30000});
  const read = (file: string) => readFileSync(path.join(folder, "nested/output", file), "utf8");

  beforeEach(() => {
    folder = mkdtempSync(path.join(tmpdir(), "abaplint-layout-"));
    mkdirSync(path.join(folder, "node_modules/@abaplint"), {recursive: true});
    symlinkSync(realpathSync(path.resolve("packages/runtime")), path.join(folder, "node_modules/@abaplint/runtime"), "junction");
    mkdirSync(path.join(folder, "src"));
    mkdirSync(path.join(folder, "extra"));
  });

  afterEach(() => {
    rmSync(folder, {recursive: true, force: true});
  });

  it("executes project modules and tests across two libraries with maps and binary assets", () => {
    write("deps/base/src/#demo#cl_base.clas.abap", [
      "CLASS /demo/cl_base DEFINITION PUBLIC.",
      "PUBLIC SECTION. CLASS-DATA value TYPE i VALUE 7. ENDCLASS.",
      "CLASS /demo/cl_base IMPLEMENTATION. ENDCLASS.",
    ].join("\n"));
    write("deps/middle/src/zcl_middle.clas.abap", [
      "CLASS zcl_middle DEFINITION PUBLIC INHERITING FROM /demo/cl_base. ENDCLASS.",
      "CLASS zcl_middle IMPLEMENTATION. ENDCLASS.",
    ].join("\n"));
    write("deps/base/src/#demo#cl_base.clas.testclasses.abap", "invalid dependency test file must be excluded");
    write("src/zcl_app.clas.abap", [
      "CLASS zcl_app DEFINITION PUBLIC INHERITING FROM zcl_middle.",
      "PUBLIC SECTION. CLASS-METHODS run. ENDCLASS.",
      "CLASS zcl_app IMPLEMENTATION. METHOD run. lcl_helper=>run( ). ENDMETHOD. ENDCLASS.",
    ].join("\n"));
    write("src/zcl_app.clas.locals_def.abap",
      "CLASS lcl_helper DEFINITION. PUBLIC SECTION. CLASS-METHODS run. ENDCLASS.");
    write("src/zcl_app.clas.locals_imp.abap",
      "CLASS lcl_helper IMPLEMENTATION. METHOD run. ASSERT zcl_middle=>value = 7. ENDMETHOD. ENDCLASS.");
    write("src/zcl_app.clas.testclasses.abap", [
      "CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.",
      "PRIVATE SECTION. METHODS test FOR TESTING. ENDCLASS.",
      "CLASS ltcl_test IMPLEMENTATION. METHOD test. zcl_app=>run( ). ENDMETHOD. ENDCLASS.",
    ].join("\n"));
    write("extra/zapp.prog.abap", "zcl_app=>run( ).");
    const bytes = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x00, 0x80, 0xff]);
    write("deps/base/src/zimage%2epng.w3mi.xml", [
      '<abapGit><asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0"><asx:values>',
      '<NAME>ZIMAGE.PNG</NAME><TEXT>test</TEXT><PARAMS/></asx:values></asx:abap></abapGit>',
    ].join(""));
    write("deps/base/src/zimage%2epng.w3mi.data.png", bytes);
    write("src/zfont.smim.xml",
      '<abapGit><asx:abap xmlns:asx="http://www.sap.com/abapxml"><asx:values>' +
      '<URL>/font.woff</URL><CLASS>font/woff</CLASS></asx:values></asx:abap></abapGit>');
    write("src/zfont.smim.data.woff", bytes);
    write("deps/middle/src/zfunctions.fugr.xml",
      '<abapGit><asx:abap xmlns:asx="http://www.sap.com/abapxml"><asx:values>' +
      '<AREAT>test</AREAT><INCLUDES><SOBJ_NAME>LZFUNCTIONSTOP</SOBJ_NAME><SOBJ_NAME>SAPLZFUNCTIONS</SOBJ_NAME></INCLUDES>' +
      '<FUNCTIONS><item><FUNCNAME>ZLAYOUT_TEST</FUNCNAME><SHORT_TEXT>test</SHORT_TEXT></item></FUNCTIONS>' +
      '</asx:values></asx:abap></abapGit>');
    for (const [name, type] of [["lzfunctionstop", "I"], ["saplzfunctions", "F"]]) {
      write("deps/middle/src/zfunctions.fugr." + name + ".xml",
        '<abapGit><asx:abap xmlns:asx="http://www.sap.com/abapxml"><asx:values><PROGDIR><NAME>' +
        name.toUpperCase() + '</NAME><SUBC>' + type + '</SUBC></PROGDIR></asx:values></asx:abap></abapGit>');
    }
    write("deps/middle/src/zfunctions.fugr.lzfunctionstop.abap", "FUNCTION-POOL zfunctions.");
    write("deps/middle/src/zfunctions.fugr.saplzfunctions.abap", "INCLUDE lzfunctionstop.\nINCLUDE lzfunctionsuxx.");
    write("deps/middle/src/zfunctions.fugr.zlayout_test.abap", "FUNCTION zlayout_test. ASSERT 1 = 1. ENDFUNCTION.");
    write("extra/zapp.prog.abap", "zcl_app=>run( ).\nCALL FUNCTION 'ZLAYOUT_TEST'.");
    write("deps/base/src/package.devc.xml", "<abapGit/>");
    write("deps/middle/src/package.devc.xml", "<abapGit/>");
    const settings = config([
      {folder: "deps/base", name: "base%#"},
      {folder: "deps/middle"},
    ]);
    write("nested/output/test-setup.mjs", [
      "export function setup() {",
      "  abap.Classes.KERNEL_UNIT_RUNNER = {run: async ({it_input}) => {",
      "    if (it_input.array().length !== 1) throw new Error('Expected one project test');",
      "    const {ltcl_test} = await import('./project/zcl_app.clas.testclasses.mjs');",
      "    const test = await new ltcl_test().constructor_();",
      "    await test.FRIENDS_ACCESS_INSTANCE.test();",
      "    const list = it_input.clone(); list.clear();",
      "    return new abap.types.Structure({list, json: new abap.types.String().set(JSON.stringify({ok: true}))});",
      "  }};",
      "}",
    ].join("\n"));
    settings.options = {...settings.options, setup: {filename: "./test-setup.mjs", postFunction: "setup"}};
    build(settings);
    expect(existsSync(path.join(folder, "nested/output/zapp.prog.mjs"))).to.equal(false);
    expect(read("project/zapp.prog.mjs")).to.contain('import("../_init.mjs")');
    expect(read("project/zcl_app.clas.mjs")).to.contain('import("../middle/zcl_middle.clas.mjs")');
    expect(read("middle/zcl_middle.clas.mjs")).to.contain('import("../base%25%23/%23demo%23cl_base.clas.mjs")');
    expect(read("init.mjs")).to.contain('./project/zcl_app.clas.mjs');
    expect(read("_unit_open.mjs")).to.contain('./project/zcl_app.clas.testclasses.mjs');
    expect(read("base%#/zimage%2epng.w3mi.mjs")).to.contain("base%#/zimage%2epng.w3mi.data.png");
    expect(readFileSync(path.join(folder, "nested/output/base%#/zimage%2epng.w3mi.data.png")).equals(bytes)).to.equal(true);
    const map = JSON.parse(read("project/zapp.prog.mjs.map"));
    expect(map.file).to.equal("zapp.prog.mjs");
    expect(map.sources).to.deep.equal(["../../../extra/zapp.prog.abap"]);
    expect(map.sourcesContent).to.deep.equal(["zcl_app=>run( ).\nCALL FUNCTION 'ZLAYOUT_TEST'."]);
    expect(read("project/zapp.prog.mjs")).to.contain("sourceMappingURL=zapp.prog.mjs.map");
    expect(read("project/zfont.smim.mjs")).to.contain("project/zfont.smim.data.woff");
    expect(readFileSync(path.join(folder, "nested/output/project/zfont.smim.data.woff")).equals(bytes)).to.equal(true);
    expect(read("init.mjs")).to.contain("./middle/zfunctions.fugr.mjs");
    run("nested/output/project/zapp.prog.mjs");
    run("nested/output/init.mjs");
    expect(run("nested/output/index.mjs")).to.contain("running ltcl_test->test");
    run("nested/output/_unit_open.mjs");
    expect(JSON.parse(read("output.json"))).to.deep.equal({ok: true});
    const before = read("project/zcl_app.clas.mjs");
    build(settings);
    expect(read("project/zcl_app.clas.mjs")).to.equal(before);
  });

  it("groups project inputs without libraries or maps", () => {
    write("src/zfirst.prog.abap", "ASSERT 1 = 1.");
    write("extra/zsecond.prog.abap", "ASSERT 2 = 2.");
    build(config([], false));
    expect(existsSync(path.join(folder, "nested/output/project/zfirst.prog.mjs"))).to.equal(true);
    expect(existsSync(path.join(folder, "nested/output/project/zsecond.prog.mjs"))).to.equal(true);
    expect(existsSync(path.join(folder, "nested/output/project/zfirst.prog.mjs.map"))).to.equal(false);
    run("nested/output/project/zfirst.prog.mjs");
    run("nested/output/index.mjs");
  });

  it("skips identical output and removes tracked modules, maps, and test runners", () => {
    write("src/zfirst.prog.abap", "ASSERT 1 = 1.");
    const settings = {...config(), incremental_output: true};
    const first = build(settings);
    expect(first).to.contain("7 created");
    write("nested/output/custom.txt", "keep");

    const module = path.join(folder, "nested/output/project/zfirst.prog.mjs");
    const before = readFileSync(module, "utf8");
    const second = build(settings);
    expect(second).to.contain("7 unchanged");
    expect(readFileSync(module, "utf8")).to.equal(before);

    rmSync(path.join(folder, "src/zfirst.prog.abap"));
    const withoutRunners = {...settings, write_source_map: false, write_unit_tests: false};
    const third = build(withoutRunners);
    expect(third).to.contain("4 deleted");
    expect(existsSync(module)).to.equal(false);
    expect(existsSync(module + ".map")).to.equal(false);
    expect(existsSync(path.join(folder, "nested/output/index.mjs"))).to.equal(false);
    expect(existsSync(path.join(folder, "nested/output/_unit_open.mjs"))).to.equal(false);
    expect(read("custom.txt")).to.equal("keep");
  });

  it("maps program errors to the original ABAP line after the bootstrap", () => {
    write("src/zbad.prog.abap", "WRITE 'before'.\nASSERT 1 = 2.");
    build(config());
    let stack = "";
    try {
      execFileSync(process.execPath, ["--enable-source-maps", "nested/output/project/zbad.prog.mjs"],
                   {cwd: folder, encoding: "utf8", stdio: "pipe"});
    } catch (error) {
      stack = String(error.stderr).replace(/\\/g, "/");
    }
    expect(stack).to.contain("/src/zbad.prog.abap:2:");
  });

  it("embeds cloned library sources after deleting the checkout", () => {
    write("src/zapp.prog.abap", "ASSERT 1 = 1.");
    write("repo/src/zcl_dep.clas.abap",
      "CLASS zcl_dep DEFINITION PUBLIC. ENDCLASS. CLASS zcl_dep IMPLEMENTATION. ENDCLASS.");
    const repo = path.join(folder, "repo");
    for (const args of [
      ["init"], ["add", "."],
      ["-c", "user.name=Layout test", "-c", "user.email=layout@example.invalid", "commit", "-m", "fixture"],
    ]) {
      execFileSync("git", args, {cwd: repo, stdio: "pipe"});
    }
    build(config([{url: repo, name: "cloned"}]));
    const mapText = read("cloned/zcl_dep.clas.mjs.map");
    const map = JSON.parse(mapText);
    expect(map.sourcesContent[0]).to.contain("CLASS zcl_dep");
    expect(map.sources).to.deep.equal(["abaplint://cloned/src/zcl_dep.clas.abap"]);
    expect(mapText).not.to.contain("abap_transpile-");
    expect(mapText).not.to.contain(path.resolve(folder).replace(/\\/g, "/"));

    const incremental = {...config([{url: repo, name: "cloned"}]), incremental_output: true};
    build(incremental);
    expect(read("cloned/zcl_dep.clas.mjs.map")).to.equal(mapText);
    run("nested/output/init.mjs");
  });
});

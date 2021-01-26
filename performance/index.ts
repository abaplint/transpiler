import {AsyncFunction, runFiles} from "../test/_utils";
import {ABAP} from "../packages/runtime/src";
import {performance} from "perf_hooks";
import * as fs from "fs";
import * as path from "path";

// NOTES
// * does not run via Mocha
// * Each test targeted to run around 1-2 seconds

const abap: ABAP = new ABAP();

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

type Tests = {name: string, abap: string}[];
type Results = {name: string, runtime: number}[];

const tests: Tests = [
  {name: "Internal table, APPEND and DELETE", abap: `
FORM run.
  CONSTANTS c_max TYPE i VALUE 5000000.
  DATA lv_index TYPE i.
  DATA table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DO c_max TIMES.
    APPEND sy-index TO table.
  ENDDO.
  ASSERT lines( table ) = c_max.
  WHILE lines( table ) > 0.
    lv_index = lines( table ).
    DELETE table INDEX lv_index.
  ENDWHILE.
  ASSERT lines( table ) = 0.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`},

  {name: "Internal table, READ TABLE, table_line", abap: `
FORM run.
  CONSTANTS c_max TYPE i VALUE 20000.
  DATA str TYPE string.
  DATA table TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  DO c_max TIMES.
    str = |foobar{ sy-index }|.
    APPEND str TO table.
  ENDDO.
  ASSERT lines( table ) = c_max.

  DO 1000 TIMES. " make sure READ TABLE takes the most time
    READ TABLE table WITH KEY table_line = str TRANSPORTING NO FIELDS.
    ASSERT sy-tabix = c_max.
  ENDDO.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`},
];

async function execute() {
  const result: Results = [];
  for (const t of tests) {
    const js = await run(t.abap);
    const f = new AsyncFunction("abap", js);

    const t0 = performance.now();
    await f(abap);
    const t1 = performance.now();

    result.push({name: t.name, runtime: Math.round(t1 - t0)});
  }
  return result;
}

async function start() {
  const time = new Date().toLocaleTimeString([], {hour: "2-digit", minute: "2-digit", hour12: false});
  console.log("START PERFORMANCE TEST, " + time);
  const results = await execute();
  let index = 1;
  for (const r of results) {
    console.log(`${ (index++ + "").padStart(3, "0") }: ${ r.name.padEnd(50, " ") } ${ r.runtime }ms`);
  }
  fs.writeFileSync(__dirname + path.sep + "results.json", JSON.stringify(results, null, 2));
}

start().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});
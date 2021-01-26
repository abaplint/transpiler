'use strict';
const fs = require("fs");

const after = JSON.parse(fs.readFileSync("./results_after.json", "utf-8"));
const before = JSON.parse(fs.readFileSync("./results_before.json", "utf-8"));

const result = {};

for (const a of after) {
  if (result[a.name] === undefined) {
    result[a.name] = {};
  }
  result[a.name].after = a.runtime;
}

for (const b of before) {
  if (result[b.name] === undefined) {
    result[b.name] = {};
  }
  result[b.name].before = b.runtime;
}

console.dir(results);

let comment = "Regression test results: hello world\n";

console.dir(comment);

fs.writeFileSync("comment-body.txt", comment);
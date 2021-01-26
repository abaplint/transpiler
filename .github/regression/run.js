'use strict';
const fs = require("fs");

console.dir(JSON.parse(fs.readFileSync("./results_after.json", "utf-8")));

let comment = "hello world";

console.dir(comment);

fs.writeFileSync("comment-body.txt", comment);
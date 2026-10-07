/* eslint-disable @typescript-eslint/no-require-imports */
/* eslint-disable @typescript-eslint/no-var-requires */
const path = require("path");

// The CLI builds the registry and the transpiler checks it with instanceof, so both must share one
// @abaplint/core. A linked transpiler (npm link) would otherwise bundle its own nested copy.
const transpiler = path.dirname(require.resolve("@abaplint/transpiler/package.json"));
const core = path.dirname(require.resolve("@abaplint/core/package.json", {paths: [transpiler]}));

module.exports = {
  resolve: {
    alias: {
      "@abaplint/core": core,
    },
  },
  entry: "./build/index.js",
  mode: "development",
  devtool: false,
  target: "node",
  output: {
    filename: "bundle.js",
    path: path.resolve(__dirname, "build"),
  },
};
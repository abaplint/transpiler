/* eslint-disable @typescript-eslint/no-require-imports */
/* eslint-disable @typescript-eslint/no-var-requires */
const path = require("path");
const webpack = require("webpack");
const HtmlWebPackPlugin = require("html-webpack-plugin");

module.exports = {
  mode: "development",
  entry: {
    app: "./src/index.ts",
    "editor.worker": "monaco-editor/esm/vs/editor/editor.worker.js",
    "json.worker": "monaco-editor/esm/vs/language/json/json.worker",
    "ts.worker": "monaco-editor/esm/vs/language/typescript/ts.worker",
  },
  devServer: {
    open: true,
    hot: true,
  },
  resolve: {
    fallback: {
      "buffer": require.resolve("buffer/"),
      "stream": require.resolve("stream-browserify"),
      "crypto": false,
      "path": require.resolve("path-browserify"),
      "fs": false,
      "http": false,
      "https": false,
      "zlib": false,
      "util": false,
      "url": false,
      "string_decoder": require.resolve("string_decoder/"),
    },
    extensions: [".ts", ".js"],
    alias: {
      "@abaplint/runtime": "../../../packages/runtime/build/src/",
      "@abaplint/transpiler": "../../../packages/transpiler/build/src/",
    },
  },
  output: {
    globalObject: "self",
    filename: "[name].bundle.js",
    path: path.resolve(__dirname, "dist"),
  },
  module: {
    rules: [
      {
        test: /\.ts?$/,
        use: "ts-loader",
        exclude: /node_modules/,
      },
      {
        test: /\.css$/,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.png$/,
        include: /favicon/,
        use: "file-loader?name=[name].[ext]",
      },
      {
        test: /\.png$|\.svg$/,
        exclude: /favicon/,
        use: "url-loader?limit=1024",
      },
      {
        test: /\.ttf$/,
        type: "asset/resource",
        generator: {
          filename: "[name][ext]",
        },
      },
    ],
  },
  plugins: [
    new HtmlWebPackPlugin({
      template: "public/index.html",
    }),
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
    }),
  ],
};

import * as path from "path";
import * as webpack from "webpack";
import * as memfs from "memfs";

export default function run(fixture: any, options = {}) {
  const compiler = webpack({
    context: __dirname,
    target: "node",
    entry: `./${fixture}`,
    devtool: "nosources-source-map",
    output: {
      path: path.resolve(__dirname),
      filename: "bundle.js",
    },
    module: {
      rules: [{
        test: /\.abap$/,
        use: {
          loader: path.resolve(__dirname, "../src/loader.js"),
          options,
        },
      }],
    },
  });

  const input = memfs.createFsFromVolume(memfs.Volume.fromJSON({
    "./zprogram.prog.abap": "WRITE 'sdf'.",
    "../src/loader.js": "",  // there is something strange here, it needs to be able to resolve from virtual file system, but the file is read from physical?
  }, __dirname));
  // @ts-ignore
  compiler.inputFileSystem = input;
  // @ts-ignore
  compiler.outputFileSystem = {...input, join: path.join};

  return new Promise((resolve, reject) => {
    compiler.run((err, stats) => {
      if (err) {
        reject(err);
      } else if (stats.hasErrors()) {
        reject(new Error(stats.toJson().errors.join("")));
      }

      resolve(stats);
    });
  });
}
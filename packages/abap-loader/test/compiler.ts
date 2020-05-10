import * as path from "path";
import * as webpack from "webpack";
import {createFsFromVolume, Volume} from "memfs";

export default function run(fixture: any, _options = {}) {
  const compiler = webpack({
    context: __dirname,
    entry: `./${fixture}`,
    output: {
      path: path.resolve(__dirname),
      filename: "bundle.js",
    },
    module: {
      rules: [{
        test: /\.abap$/,
        use: {
          loader: path.resolve(__dirname, "../src/loader.js"),
        },
      }],
    },
  });

  const volume = createFsFromVolume(new Volume());
  // @ts-ignore
  compiler.outputFileSystem = {...volume, join: path.join};

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
import * as path from "path";
import * as webpack from "webpack";
import * as memfs from "memfs";

export interface IFixture {
  entryFilename: string,
  files: {filename: string, contents: string}[],
}

export function run(fixture: IFixture, options = {}): Promise<webpack.Stats> {
  const compiler = webpack({
    context: __dirname,
    target: "node",
    entry: `./${fixture.entryFilename}`,
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

  // there is something strange with "loader.js", it needs to be able to resolve from virtual file system,
  // but the file is read from physical?
  const jsonFiles: any = {"../src/loader.js": ""};
  for (const f of fixture.files) {
    jsonFiles[f.filename] = f.contents;
  }

  const input = memfs.createFsFromVolume(memfs.Volume.fromJSON(jsonFiles, __dirname));
  // @ts-ignore
  compiler.inputFileSystem = input;
  // @ts-ignore
  compiler.outputFileSystem = {...input, join: path.join};

  return new Promise((resolve, reject) => {
    compiler.run((err, stats) => {
      if (err) {
        reject(err);
      } else if (stats?.hasErrors()) {
        reject(new Error(stats.toJson().errors.join("")));
      }

      resolve(stats);
    });
  });
}
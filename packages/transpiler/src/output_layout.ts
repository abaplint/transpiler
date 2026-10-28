import * as abaplint from "@abaplint/core";
import {IObjectIdentifier} from "./types";

/** Source filenames to output folders. Omit this map to retain flat output. */
export type OutputFolders = ReadonlyMap<string, string>;

/** Browser-safe paths: generated modules use POSIX paths on every platform. */
export class OutputLayout {
  private readonly grouped: boolean;
  private readonly objectFolders = new Map<string, string>();
  private readonly sourceFolders = new Map<string, string>();

  public constructor(reg?: abaplint.IRegistry, folders?: OutputFolders) {
    this.grouped = folders !== undefined;
    if (folders === undefined) {
      return;
    }
    for (const [filename, folder] of folders) {
      this.sourceFolders.set(filename.toLowerCase(), folder);
    }
    for (const obj of reg?.getObjects() || []) {
      const owners = new Set(obj.getFiles().map(f => this.sourceFolders.get(f.getFilename().toLowerCase()))
        .filter((folder): folder is string => folder !== undefined));
      if (owners.size !== 1) {
        throw new Error("Ambiguous output ownership for " + obj.getType() + " " + obj.getName());
      }
      const folder = [...owners][0];
      this.objectFolders.set(this.key({type: obj.getType(), name: obj.getName()}), folder);
      for (const file of obj.getFiles()) {
        // Class locals are merged into a single module.
        this.sourceFolders.set(file.getFilename().toLowerCase().replace(/\.locals_(def|imp)\.abap$/, ".locals.abap"), folder);
      }
    }
  }

  private key(obj: IObjectIdentifier): string {
    return obj.type.toUpperCase() + ":" + obj.name.toUpperCase();
  }

  public file(obj: IObjectIdentifier, filename: string): string {
    const folder = this.objectFolders.get(this.key(obj));
    if (this.grouped && folder === undefined) {
      throw new Error("Missing output ownership for " + obj.type + " " + obj.name);
    }
    if (folder !== undefined && (filename.startsWith("/") || /[\\:]/.test(filename)
        || filename.split("/").some(segment => segment === "" || segment === "." || segment === ".."))) {
      throw new Error("Invalid output filename: " + filename);
    }
    return folder === undefined ? filename : folder + "/" + filename;
  }

  public sourceModule(filename: string): string {
    const folder = this.sourceFolders.get(filename.toLowerCase());
    if (this.grouped && folder === undefined) {
      throw new Error("Missing output ownership for source " + filename);
    }
    const module = filename.replace(/\.abap$/i, ".mjs").toLowerCase();
    return folder === undefined ? module : folder + "/" + module;
  }

  public objectModule(obj: abaplint.IObject, suffix = ""): string {
    const identifier = {name: obj.getName(), type: obj.getType()};
    const filename = obj.getName().toLowerCase().replace(/\//g, "#") + "." + obj.getType().toLowerCase() + suffix + ".mjs";
    return this.file(identifier, filename);
  }
}

/** Escape each segment without turning directory separators into namespace markers. */
export function importPath(from: string, to: string): string {
  const source = from.split("/").slice(0, -1);
  const target = to.split("/");
  while (source.length > 0 && target.length > 0 && source[0] === target[0]) {
    source.shift();
    target.shift();
  }
  const relative = [...source.map(() => ".."), ...target].join("/");
  const escaped = relative.split("/").map(segment => encodeURIComponent(segment)).join("/");
  return escaped.startsWith("../") ? escaped : "./" + escaped;
}

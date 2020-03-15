import {MemoryFile, Registry, ABAPObject, SyntaxLogic} from "abaplint";
import {Validation} from "./validation";
import {Indentation} from "./indentation";
import {Traversal} from "./traversal";

export interface IFile {
  filename: string,
  contents: string,
}

export interface IOutput {
  js: IFile[];
  maps: IFile[];
}

export interface ITranspilerOptions {
  ignoreSyntaxCheck?: boolean;
  includeUnitTests?: boolean;
}

export class Transpiler {
  private readonly options: ITranspilerOptions | undefined;

  public constructor(options?: ITranspilerOptions) {
    this.options = options;
  }

  public async run(files: IFile[]): Promise<IOutput> {
    const memory = files.map(f => new MemoryFile(f.filename, f.contents));
    const reg = new Registry().addFiles(memory);
    this.validate(reg);

    const output: IOutput = {js: [], maps: []};
    for (const abap of reg.getABAPObjects()) {
      const res = this.runObject(abap, reg);
      output.js = output.js.concat(res.js);
      output.maps = output.maps.concat(res.maps);
    }
    return output;
  }

// ///////////////////////////////

  protected runObject(obj: ABAPObject, reg: Registry): IOutput {
    const output: IOutput = {js: [], maps: []};

    const spaghetti = new SyntaxLogic(reg, obj).run().spaghetti;

    for (const f of obj.getABAPFiles()) {
      let contents = new Traversal(spaghetti, f.getFilename()).traverse(f.getStructure());

      if (contents.endsWith("\n")) {
        contents = contents.substring(0, contents.length - 1);
      }

      if (contents.length > 0) {
        const filename = f.getFilename().replace(new RegExp("\.abap$"), ".js");
        contents = new Indentation().run(contents);
        output.js.push({filename, contents});
      }
    }

    return output;
  }

  protected validate(reg: Registry) {
    const issues = new Validation(this.options).run(reg);
    if (issues.length > 0) {
      const messages = issues.map(i => i.getMessage());
      throw new Error(messages.join("\n"));
    }
  }

}
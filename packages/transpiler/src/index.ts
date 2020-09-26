import * as abaplint from "@abaplint/core";
import {Validation, config} from "./validation";
import {Indentation} from "./indentation";
import {Traversal} from "./traversal";

export {config};

export interface IFile {
  filename: string,
  contents: string,
}

export interface IObjectIdentifier {
  name: string,
  type: string,
}

export interface IOutput {
  object: IObjectIdentifier;
  requires: readonly IObjectIdentifier[];
  js: IFile; // one javascript output file for each object
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

  public async run(files: IFile[]): Promise<IOutput[]> {
    const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
    const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
    this.validate(reg);

    const output: IOutput[] = [];

    for (const abap of reg.getObjects()) {
      if (abap.getType() === "INTF") {
        continue;
      }
      if (abap instanceof abaplint.ABAPObject) {
        const res = this.runObject(abap, reg);
        if (res) {
          output.push(res);
        }
      }
    }

    return output;
  }

// ///////////////////////////////

  protected runObject(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): IOutput | undefined {
    const spaghetti = new abaplint.SyntaxLogic(reg, obj).run().spaghetti;

    const file = obj.getMainABAPFile(); // todo, start with the right ABAP file
    if (file === undefined) {
      return undefined;
    }

    let contents = new Traversal(spaghetti, file, obj).traverse(file.getStructure());

    if (contents.endsWith("\n")) {
      contents = contents.substring(0, contents.length - 1);
    }

    if (contents.length > 0) {
      const filename = file.getFilename().replace(new RegExp("\.abap$"), ".js");
      contents = new Indentation().run(contents);

      const output: IOutput = {
        object: {name: obj.getName(), type: obj.getType()},
        js: {filename, contents},
        requires: this.findRequires(spaghetti.getTop(), reg),
      };

      return output;
    }

    return undefined;
  }

  protected findRequires(node: abaplint.ISpaghettiScopeNode, reg: abaplint.IRegistry): IObjectIdentifier[] {
    let ret: IObjectIdentifier[] = [];

    for (const v of node.getData().vars) {
      const type = v.identifier.getType();
      if (v.identifier.getName() !== "me" // todo, this is a hack
          && type instanceof abaplint.BasicTypes.ObjectReferenceType) {
        const found = reg.getObject("CLAS", type.getName());
        if (found) {
          ret.push({type: found.getType(), name: found.getName()});
        }
      }
    }

    for (const c of node.getChildren()) {
      ret = ret.concat(this.findRequires(c, reg));
    }

    return ret;
  }

  protected validate(reg: abaplint.IRegistry): void {
    const issues = new Validation(this.options).run(reg);
    if (issues.length > 0) {
      const messages = issues.map(i => i.getMessage() + ", " + i.getFilename() + ", " + i.getStart().getRow());
      throw new Error(messages.join("\n"));
    }
  }

}
import {Nodes, MemoryFile, Registry, Structures, ABAPObject, SyntaxLogic} from "abaplint";
import {Validation} from "./validation";
import * as StatementTranspilers from "./statements";
import {Indentation} from "./indentation";

export interface IFile {
  filename: string,
  contents: string,
}

export interface IOutput {
  js: IFile[];
  maps: IFile[];
}

export interface ITranspilerOptions {
  ignoreSyntaxCheck: boolean;
}

export class Transpiler {
  private readonly options: ITranspilerOptions | undefined;

  public constructor(options?: ITranspilerOptions) {
    this.options = options;
  }

  public async runMulti(files: IFile[]): Promise<IOutput> {
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

// todo, deprecate/remove this method
  public run(code: string): string {
    const reg = new Registry().addFile(new MemoryFile("zfoobar.prog.abap", code));

    this.validate(reg);

    const res = this.runObject(reg.getABAPObjects()[0], reg).js[0]?.contents;

    return res ? res : "";
  }

// ///////////////////////////////

  protected runObject(obj: ABAPObject, reg: Registry): IOutput {
    const output: IOutput = {js: [], maps: []};

    new SyntaxLogic(reg, obj).run();
    /*
    const syntax =
    console.dir(syntax.issues.length);
*/
    for (const f of obj.getABAPFiles()) {
      let contents = this.traverseStructure(f.getStructure()!);

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

  protected traverseStructure(node: Nodes.StructureNode): string {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c instanceof Nodes.StructureNode) {
        if (c.get() instanceof Structures.Interface
            || c.get() instanceof Structures.ClassDefinition) {
          continue;
        }
        ret = ret + this.traverseStructure(c);
        if (c.get() instanceof Structures.When) {
          ret = ret + "break;\n";
        }
      } else if (c instanceof Nodes.StatementNode) {
        ret = ret + this.traverseStatement(c) + "\n";
      } else {
        throw new Error("traverseStructure, unexpected node type");
      }
    }
    return ret;
  }

  protected traverseStatement(node: Nodes.StatementNode): string {
    const list: any = StatementTranspilers;
    for (const key in list) {
      const transpiler = new list[key]();
      if (node.get().constructor.name + "Transpiler" === transpiler.constructor.name) {
        return transpiler.transpile(node);
      }
    }
    throw new Error(`Statement ${node.get().constructor.name} not supported`);
  }

}
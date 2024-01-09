import * as abaplint from "@abaplint/core";
import {Validation, config} from "./validation";
import {UnitTest} from "./unit_test";
import {Keywords} from "./keywords";
import {IFile, IOutput, IProgress, ITranspilerOptions, IOutputFile, UnknownTypesEnum} from "./types";
import {DatabaseSetup} from "./db";
import {HandleTable} from "./handlers/handle_table";
import {HandleABAP} from "./handlers/handle_abap";
import {HandleDataElement} from "./handlers/handle_data_element";
import {HandleTableType} from "./handlers/handle_table_type";
import {HandleView} from "./handlers/handle_view";
import {HandleEnqu} from "./handlers/handle_enqu";
import {HandleTypePool} from "./handlers/handle_type_pool";
import {HandleW3MI} from "./handlers/handle_w3mi";
import {HandleSMIM} from "./handlers/handle_smim";
import {HandleMSAG} from "./handlers/handle_msag";
import {HandleOA2P} from "./handlers/handle_oa2p";

export {config, ITranspilerOptions, IFile, IProgress, IOutputFile, IOutput, UnknownTypesEnum};

export class Transpiler {
  private readonly options: ITranspilerOptions | undefined;

  public constructor(options?: ITranspilerOptions) {
    this.options = options;
    if (this.options === undefined) {
      this.options = {};
    }
    if (this.options.unknownTypes === undefined) {
      this.options.unknownTypes = UnknownTypesEnum.compileError;
    }
  }

  // workaround for web/webpack
  public async runRaw(files: IFile[]): Promise<IOutput> {
    const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
    const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
    return new Transpiler().run(reg);
  }

  public async run(reg: abaplint.IRegistry, progress?: IProgress): Promise<IOutput> {

    reg.parse();
    new Keywords(this.options?.keywords).handle(reg);
    this.validate(reg);

    const dbSetup = new DatabaseSetup(reg).run(this.options);

    const output: IOutput = {
      objects: [],
      unitTestScript: new UnitTest().unitTestScript(reg, this.options?.skip),
      unitTestScriptOpen: new UnitTest().unitTestScriptOpen(reg, this.options?.skip),
      initializationScript: new UnitTest().initializationScript(reg, dbSetup, this.options?.extraSetup),
      initializationScript2: new UnitTest().initializationScript(reg, dbSetup, this.options?.extraSetup, true),
      databaseSetup: dbSetup,
      reg: reg,
    };

    progress?.set(reg.getObjectCount(false), "Building");
    for (const obj of reg.getObjects()) {
      await progress?.tick("Building, " + obj.getName());
      if (obj instanceof abaplint.ABAPObject && !(obj instanceof abaplint.Objects.TypePool)) {
        output.objects.push(...new HandleABAP(this.options).runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.TypePool) {
        output.objects.push(...new HandleTypePool().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.Oauth2Profile) {
        output.objects.push(...new HandleOA2P().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.Table) {
        output.objects.push(...new HandleTable().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.View) {
        output.objects.push(...new HandleView().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.LockObject) {
        output.objects.push(...new HandleEnqu().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.DataElement) {
        output.objects.push(...new HandleDataElement().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.TableType) {
        output.objects.push(...new HandleTableType().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.MIMEObject) {
        output.objects.push(...new HandleSMIM().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.WebMIME) {
        output.objects.push(...new HandleW3MI().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.MessageClass) {
        output.objects.push(...new HandleMSAG().runObject(obj, reg));
      }
    }

    return output;
  }

// ///////////////////////////////

  protected validate(reg: abaplint.IRegistry): void {
    const issues = new Validation(this.options).run(reg);
    if (issues.length > 0) {
      const messages = issues.map(i => i.getKey() + ", " +
        i.getMessage() + ", " +
        i.getFilename() + ":" +
        i.getStart().getRow());
      throw new Error(messages.join("\n"));
    }
  }

}
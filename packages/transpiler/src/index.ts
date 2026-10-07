import * as abaplint from "@abaplint/core";
import {Validation, config} from "./validation";
import {UniqueIdentifier} from "./unique_identifier";
import {UnitTest} from "./unit_test";
import {IFile, IOutput, IProgress, ITranspilerOptions, IOutputFile, UnknownTypesEnum} from "./types";
import {Chunk} from "./chunk";
import {DatabaseSetupResult} from "./db/database_setup_result";
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
import {HandleFUGR} from "./handlers/handle_fugr";
import {Initialization} from "./initialization";
import {OutputLayout, OutputFolders} from "./output_layout";

export {OutputLayout, OutputFolders, importPath} from "./output_layout";

export {config, ITranspilerOptions, IFile, IProgress, IOutputFile, IOutput,
  UnknownTypesEnum, Chunk, DatabaseSetupResult};

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
    const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory);
    return this.run(reg);
  }

  public async run(reg: abaplint.IRegistry, progress?: IProgress, folders?: OutputFolders): Promise<IOutput> {
    // Validation installs the transpiler configuration and findIssues() parses
    // dirty registries. Parsing before that would be wasted because setConfig()
    // marks every registry object dirty again.
    this.validate(reg);
    const layout = new OutputLayout(reg, folders);

    const dbSetup = new DatabaseSetup(reg).run(this.options);

    const output: IOutput = {
      objects: [],
      unitTestScript: new UnitTest(layout).unitTestScript(reg, this.options?.skip),
      unitTestScriptOpen: new UnitTest(layout).unitTestScriptOpen(reg, this.options?.skip),
      initializationScript: "",
      initializationScript2: "",
      databaseSetup: dbSetup,
      reg: reg,
    };

    progress?.set(reg.getObjectCount().total, "Building");
    for (const obj of reg.getObjects()) {
      await progress?.tick("Building, " + obj.getName());
      // the temporary names ("unique1", ..., and the DO/WHILE sy-index backups
      // "indexBackup1", ...) are local to the module an object becomes, so each
      // object numbers its own: its output then depends on the registry alone,
      // not on the objects built before it or on an earlier run in the same process
      UniqueIdentifier.reset();
      UniqueIdentifier.resetIndexBackup();
      if (obj instanceof abaplint.Objects.TypePool) {
        output.objects.push(...new HandleTypePool().runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.FunctionGroup) {
        output.objects.push(...new HandleFUGR(this.options).runObject(obj, reg));
      } else if (obj instanceof abaplint.ABAPObject) {
        output.objects.push(...new HandleABAP(this.options, layout).runObject(obj, reg));
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
        output.objects.push(...new HandleSMIM(layout).runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.WebMIME) {
        output.objects.push(...new HandleW3MI(layout).runObject(obj, reg));
      } else if (obj instanceof abaplint.Objects.MessageClass) {
        output.objects.push(...new HandleMSAG().runObject(obj, reg));
      }
    }

    output.initializationScript = new Initialization(layout).script(reg, dbSetup, this.options, false);
    output.initializationScript2 = new Initialization(layout).script(reg, dbSetup, this.options, true);

    for (const file of output.objects) {
      file.filename = layout.file(file.object, file.filename);
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

import {Issue, IRegistry, Config, IConfig, Version} from "@abaplint/core";
import {ITranspilerOptions} from "./types";

export const config: IConfig = {
  "global": {
    "files": "/**/*.*",
    "skipGeneratedGatewayClasses": true,
    "skipGeneratedPersistentClasses": true,
    "skipGeneratedFunctionGroups": true,
  },
  "syntax": {
    "version": Version.OpenABAP,
    "errorNamespace": ".",
  },
  "rules": {
    "when_others_last": true,
    "avoid_use": {
      "define": true,
      "execSQL": true,
      "kernelCall": true,
      "communication": true,
      "systemCall": true,
      "break": true,
      "statics": true,
      "endselect": false,
      "defaultKey": false,
    },
    "parser_error": true,
    "allowed_object_types": {
      "allowed": ["INTF", "CLAS", "PROG", "DEVC", "TABL", "SHLP", "XSLT", "SHMA", "SICF", "NROB","TYPE", "DTEL", "DOMA", "TTYP", "MSAG", "FUGR"],
    },
    "exit_or_check": {
      "allowCheck": true,
      "allowExit": false,
    },
    "unknown_types": true,
    "ambiguous_statement": true,
    "implement_methods": true,
    "begin_end_names": true,
    "no_chained_assignment": true,
    "check_syntax": true,
    "form_no_dash": true,
    "omit_preceding_zeros": true,
    "obsolete_statement": {
      "compute": true,
      "requested": true,
      "setExtended": true,
      "occurs": true,
    },
    "forbidden_identifier": {
      "check": [
        "^abstract$",	"^arguments$", "^await$",
        "^break$",	"^byte$", "^catch$",
        "^char$",	"^class$", "^const$", "^continue$",
        "^debugger$",	"^default$", "^do$",
        "^double$",	"^else$", "^enum$", "^eval$",
        "^export$",	"^extends$", "^false$", "^final$",
        "^finally$", "^for$", "^function$",
        "^goto$",	"^if$", "^implements$", "^import$",
        "^in$",	"^instanceof$", "^interface$",
        "^let$",	"^long$", "^native$", "^new$",
        "^null$",	"^package$", "^private$",
//        "^protected$",
        "^public$",	"^return$", "^short$", "^static$",
        "^switch$", "^synchronized$", "^this$",
        "^throw$",	"^throws$", "^transient$", "^true$",
        "^try$",	"^typeof$", "^var$", "^void$",
        "^volatile$",	"^while$", "^yield$",
        "^unique\\d+$", "^constant_\\d+$"],
    },
  },
};


// todo, make sure nothing is overloaded, eg "lines()", there is a rule for this in abaplint now

export class Validation {

  private readonly options: ITranspilerOptions | undefined;

  public constructor(options?: ITranspilerOptions) {
    this.options = options;
  }

  public run(reg: IRegistry): readonly Issue[] {
    if (this.options?.ignoreSyntaxCheck === true) {
      config.rules["check_syntax"] = false;
    } else {
      config.rules["check_syntax"] = true;
    }
    if (this.options?.unknownTypes === "runtimeError") {
      config.syntax.errorNamespace = "VOID_EVERYTHING"; // this is not a constant, just a regex that happens to not match anything
//      config.rules["unknown_types"] = false;
    }
    const conf = new Config(JSON.stringify(config));
    reg.setConfig(conf);
    const issues = reg.findIssues();
    return issues;
  }

}
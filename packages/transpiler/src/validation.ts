import {Issue, IRegistry, Config, IConfig, Version} from "@abaplint/core";
import {ITranspilerOptions} from ".";

export const config: IConfig = {
  "global": {
    "files": "/**/*.*",
    "skipGeneratedGatewayClasses": true,
    "skipGeneratedPersistentClasses": true,
    "skipGeneratedFunctionGroups": true,
  },
  "syntax": {
    "version": Version.v702,
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
      "allowed": ["INTF", "CLAS", "PROG", "DEVC", "TABL", "DTEL", "DOMA", "TTYP", "MSAG", "FUGR"],
    },
    "unknown_types": true,
    "ambiguous_statement": true,
    "implement_methods": true,
    "begin_end_names": true,
    "check_syntax": true,
    "form_no_dash": true,
    "obsolete_statement": {
      "refresh": true,
      "compute": true,
      "requested": true,
      "setExtended": true,
      "occurs": true,
      "add": false,
      "divide": false,
      "move": false,
      "multiply": false,
      "subtract": false,
    },
    "forbidden_identifier": {
      "check": [
        "^abstract$",	"^arguments$", "^await$", "^boolean$",
        "^break$",	"^byte$", "^case$", "^catch$",
        "^char$",	"^class$", "^const$", "^continue$",
        "^debugger$",	"^default$", "^do$",
        "^double$",	"^else$", "^enum$", "^eval$",
        "^export$",	"^extends$", "^false$", "^final$",
        "^finally$",	"^float$", "^for$", "^function$",
        "^goto$",	"^if$", "^implements$", "^import$",
        "^in$",	"^instanceof$", "^int$", "^interface$",
        "^let$",	"^long$", "^native$", "^new$",
        "^null$",	"^package$", "^private$", "^protected$",
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
      config.syntax.errorNamespace = "";
      config.rules["unknown_types"] = false;
    }
    const conf = new Config(JSON.stringify(config));
    reg.setConfig(conf);
    const issues = reg.findIssues();
    return issues;
  }

}
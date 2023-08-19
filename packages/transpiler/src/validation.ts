import {Issue, IRegistry, Config, IConfig, Version} from "@abaplint/core";
import {defaultKeywords} from "./keywords";
import {ITranspilerOptions, UnknownTypesEnum} from "./types";

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
      "execSQL": true,
      "kernelCall": true,
      "communication": true,
      "systemCall": true,
      "break": false,
      "statics": true,
      "endselect": false,
      "defaultKey": false,
    },
    "parser_error": true,
    "allowed_object_types": {
      "allowed": [
        "AUTH",
        "CLAS",
        "DEVC",
        "DOMA",
        "DTEL",
        "ENHS",
        "ENQU",
        "FUGR",
        "INTF",
        "MSAG",
        "NROB",
        "NSPC",
        "PARA",
        "PROG",
        "SHLP",
        "SHMA",
        "SICF",
        "SMIM",
        "SMIM",
        "SRFC",
        "SUSO",
        "TABL",
        "TOBJ",
        "TRAN",
        "TTYP",
        "TYPE",
        "VIEW",
        "W3MI",
        "XSLT",
      ],
    },
    "unknown_types": true,
    "ambiguous_statement": true,
    "implement_methods": true,
    "begin_end_names": true,
    "check_syntax": true,
    "form_no_dash": true,
    "omit_preceding_zeros": true,
    "obsolete_statement": {
      "setExtended": true,
    },
    "forbidden_identifier": {
      "check": [],
    },
  },
};


// todo, make sure nothing is overloaded, eg "lines()", there is a rule for this in abaplint now
// hmm this ^ is okay? since lines will be prefixed with "abap.builtin"?

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

    config.rules["forbidden_identifier"]["check"] = ["^unique\\d+$"];
    if (this.options?.keywords === undefined) {
      for (const d of defaultKeywords) {
        const add = "^" + d + "$";
        config.rules["forbidden_identifier"]["check"].push(add);
      }
    } else {
      for (const d of this.options.keywords) {
        const add = "^" + d + "$";
        config.rules["forbidden_identifier"]["check"].push(add);
      }
    }

    if (this.options?.unknownTypes === UnknownTypesEnum.runtimeError) {
      // this is not a constant, just a regex that happens to not match anything
      config.syntax.errorNamespace = "VOID_EVERYTHING";
    }

    const conf = new Config(JSON.stringify(config));
    reg.setConfig(conf);
    const issues = reg.findIssues();
    return issues;
  }

}
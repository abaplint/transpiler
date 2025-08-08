import {Issue, IRegistry, Config, IConfig, Version} from "@abaplint/core";
import {ITranspilerOptions, UnknownTypesEnum} from "./types";

export const config: IConfig = {
  "global": {
    "files": "/**/*.*",
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
        "APIS",
        "AUTH",
        "CLAS",
        "DEVC",
        "DOMA",
        "DTEL",
        "ENHS",
        "ENQU",
        "FUGR",
        "INTF",
        "IWMO",
        "IWOM",
        "IWPR",
        "IWSG",
        "IWSV",
        "MSAG",
        "NROB",
        "NSPC",
        "OA2P",
        "PARA",
        "PROG",
        "SHLP",
        "SHMA",
        "SICF",
        "SMIM",
        "SMIM",
        "SRFC",
        "SUSH",
        "SUSO",
        "TABL",
        "TOBJ",
        "TRAN",
        "TTYP",
        "TYPE",
        "VIEW",
        "W3MI",
        "XSLT",
        "ZN01",
        "ZN02",
        "ZN03",
        "ZN04",
        "ZN05",
        "ZN06",
        "ZN07",
        "ZN08",
        "ZN09",
        "ZN10",
        "ZN11",
        "ZN12",
        "ZN13",
        "ZN14",
        "ZN15",
        "ZN16",
        "ZN17",
        "ZN18",
        "ZN19",
        "ZN20",
        "ZN21",
        "ZN22",
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

    if (this.options?.unknownTypes === UnknownTypesEnum.runtimeError) {
      // this is not a constant, just a regex that happens to not match anything
      config.syntax.errorNamespace = "VOID_EVERYTHING";
    }

    if (this.options?.skipVersionCheck === true) {
      // todo, set it to abaplint default version
      config.syntax.version = Version.v758;
    } else {
      config.syntax.version = Version.OpenABAP;
    }

    const conf = new Config(JSON.stringify(config));
    reg.setConfig(conf);
    const issues = reg.findIssues();
    return issues;
  }

}
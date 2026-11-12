import {Issue, IRegistry, Config, IConfig, Version, SyntaxLogic, BasicTypes, ABAPObject, ISpaghettiScope, Statements} from "@abaplint/core";
import {ITranspilerOptions, UnknownTypesEnum} from "./types";

const defaultAllowedObjectTypes = [
  "APIS",
  "APLO",
  "AUTH",
  "BDEF",
  "CHKC",
  "CHKO",
  "CHKV",
  "CLAS",
  "DDLS",
  "DDLX",
  "DEVC",
  "DOMA",
  "DTEL",
  "ENHO",
  "ENHS",
  "ENQU",
  "FUGR",
  "HTTP",
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
  "PINF",
  "PROG",
  "SAJC",
  "SAJT",
  "SCO2",
  "WAPA",
  "SHLP",
  "SHMA",
  "SIA6",
  "SICF",
  "SMIM",
  "SPLO",
  "SRFC",
  "SRVB",
  "SRVD",
  "SUSC",
  "SUSH",
  "SUSO",
  "SXCI",
  "TABL",
  "TOBJ",
  "TRAN",
  "TTYP",
  "TYPE",
  "VCLS",
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
];

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
      "statics": false,
      "endselect": false,
      "defaultKey": false,
    },
    "parser_error": true,
    "allowed_object_types": {
      "allowed": defaultAllowedObjectTypes,
    },
    "unknown_types": true,
    "ambiguous_statement": true,
    "implement_methods": true,
    "begin_end_names": true,
    "check_syntax": true,
    "form_no_dash": true,
    "forbidden_identifier": {
      "check": [],
    },
  },
};


// todo, make sure nothing is overloaded, eg "lines()", there is a rule for this in abaplint now
// hmm this ^ is okay? since lines will be prefixed with "abap.builtin"?

export class Validation {

  private readonly options: ITranspilerOptions | undefined;
  private readonly scopes = new Map<ABAPObject, ISpaghettiScope>();

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
      // missing method implementations fail when called, "is not a function"
      config.rules["implement_methods"] = false;
    } else {
      config.rules["implement_methods"] = true;
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
    return issues.filter(i => i.getKey() !== "ambiguous_statement" || this.isUnambiguous(reg, i) === false);
  }

  /** DELETE name., DELETE name FROM x., MODIFY name. and MODIFY name FROM x. read two ways, so abaplint calls them
   * ambiguous. ABAP decides by the name: a variable of table type is the internal table, anything else the database table */
  private isUnambiguous(reg: IRegistry, issue: Issue): boolean {
    const file = reg.getFileByName(issue.getFilename());
    const obj = file ? reg.findObjectForFile(file) : undefined;
    if (!(obj instanceof ABAPObject)) {
      return false;
    }
    const statement = obj.getABAPFileByName(issue.getFilename())?.getStatements()
      .find(s => s.getStart().equals(issue.getStart()));
    const token = statement?.getTokens()[1];
    if (statement === undefined || token === undefined || token.getStr().toUpperCase() === "TABLE") {
      return false;
    }
    const modify = statement.getFirstToken().getStr().toUpperCase() === "MODIFY";
    if (!(statement.get() instanceof Statements.DeleteInternal) && modify === false) {
      return false;
    }
    let spaghetti = this.scopes.get(obj);
    if (spaghetti === undefined) {
      spaghetti = new SyntaxLogic(reg, obj).run().spaghetti;
      this.scopes.set(obj, spaghetti);
    }
    const variable = spaghetti.lookupPosition(token.getStart(), issue.getFilename())?.findVariable(token.getStr());
    if (variable?.getType() instanceof BasicTypes.TableType) {
      return true;
    }
    return variable !== undefined || statement.findExpressionAfterToken("FROM") !== undefined;
  }

}

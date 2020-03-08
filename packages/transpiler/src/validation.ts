import {Issue, Registry, Config, IConfig, Version} from "abaplint";
import {ITranspilerOptions} from ".";

const config: IConfig = {
  global: {
    files: "/**/*.*",
    skipGeneratedGatewayClasses: true,
    skipGeneratedPersistentClasses: true,
    skipGeneratedFunctionGroups: true,
  },
  syntax: {
    version: Version.v702,
    errorNamespace: "",
  },
  rules: {
    when_others_last: true,
    avoid_use: {
      define: true,
      execSQL: true,
      kernelCall: true,
      communication: true,
      systemCall: true,
      break: true,
      statics: true,
    },
    parser_error: true,
    check_syntax: true,
    obsolete_statement: {
      refresh: true,
      compute: true,
      requested: true,
      setExtended: true,
      occurs: true,
    },
  },
};

// todo, make sure nothing is overloaded, eg "lines()"

export class Validation {

  private readonly options: ITranspilerOptions | undefined;

  public constructor(options?: ITranspilerOptions) {
    this.options = options;
  }

  public run(reg: Registry): Issue[] {
    if (this.options?.ignoreSyntaxCheck === true) {
      config.rules["check_syntax"] = false;
    } else {
      config.rules["check_syntax"] = true;
    }
    const conf = new Config(JSON.stringify(config));
    reg.setConfig(conf);
    const issues = reg.findIssues();
    return issues;
  }

}
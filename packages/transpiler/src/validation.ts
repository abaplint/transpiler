import {Issue, IRegistry, Config, IConfig, Version} from "@abaplint/core";
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
    errorNamespace: ".",
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
    unknown_types: true,
    check_syntax: true,
    functional_writing: true,
    obsolete_statement: {
      refresh: true,
      compute: true,
      requested: true,
      setExtended: true,
      occurs: true,
    },
    forbidden_identifier: {
      check: [
        "^abstract$",	"^arguments$", "^await$", "^boolean$",
        "^break$",	"^byte$", "^case$", "^catch$",
        "^char$",	"^class$", "^const$", "^continue$",
        "^debugger$",	"^default$", "^delete$", "^do$",
        "^double$",	"^else$", "^enum$", "^eval$",
        "^export$",	"^extends$", "^false$", "^final$",
        "^finally$",	"^float$", "^for$", "^function$",
        "^goto$",	"^if$", "^implements$", "^import$",
        "^in$",	"^instanceof$", "^int$", "^interface$",
        "^let$",	"^long$", "^native$", "^new$",
        "^null$",	"^package$", "^private$", "^protected$",
        "^public$",	"^return$", "^short$", "^static$",
        "^super$",	"^switch$", "^synchronized$", "^this$",
        "^throw$",	"^throws$", "^transient$", "^true$",
        "^try$",	"^typeof$", "^var$", "^void$",
        "^volatile$",	"^while$", "^with$", "^yield$",
        "^unique\d+$"],
    },
  },
};


// todo, make sure nothing is overloaded, eg "lines()"

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
    const conf = new Config(JSON.stringify(config));
    reg.setConfig(conf);
    const issues = reg.findIssues();
    return issues;
  }

}
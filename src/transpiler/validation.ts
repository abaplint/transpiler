import {Issue, Registry, Config, IConfig, Version} from "abaplint";

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
    globalConstants: [], // todo, remove this line
    globalMacros: [], // todo, remove this line
  },
  rules: {
    when_others_last: true,
    obsolete_statement: {
      refresh: true,
      compute: true,
      requested: true,
      setExtended: true,
      occurs: true,
    },
  },
};

export class Validation {

  public static run(reg: Registry): Issue[] {
    const conf = new Config(JSON.stringify(config));
    reg.setConfig(conf);
    const issues = reg.findIssues().filter(i => i.getKey() !== "structure");
    return issues;
  }

}
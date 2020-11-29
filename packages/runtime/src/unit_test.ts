class UnitTestMethodResult {
  public result: "Pass" | "Fail" | "Skip" | undefined = undefined;

  public constructor(public readonly name: string) {
    this.result = undefined;
  }

  public pass() {
    this.result = "Pass";
  }

  public fail() {
    this.result = "Fail";
  }

  public skip() {
    this.result = "Skip";
  }
}

class UnitTestClassResult {
  public methods: UnitTestMethodResult[] = [];

  public constructor(public readonly name: string) {

  }

  public addMethod(name: string) {
    const ret = new UnitTestMethodResult(name);
    this.methods.push(ret);
    return ret;
  }
}

class UnitTestObjectResult {
  public classes: UnitTestClassResult[] = [];

  public constructor(public readonly name: string) {

  }

  public addTestClass(name: string) {
    const ret = new UnitTestClassResult(name);
    this.classes.push(ret);
    return ret;
  }
}

export class UnitTestResult {
  public objects: UnitTestObjectResult[] = [];

  public addObject(name: string) {
    const ret = new UnitTestObjectResult(name);
    this.objects.push(ret);
    return ret;
  }

  public xUnitXML(): string {
    // https://xunit.net/docs/format-xml-v2

    // <assemblies> = project
    // <assembly> = global object/global class
    // <collection> = local class
    // <test> = method

    let ret = `<?xml version="1.0" encoding="utf-8"?>\n<assemblies>\n`;
    for (const obj of this.objects) {
      ret += `  <assembly name="${obj.name}" test-framework="abap-framework" environment="abap-environment">\n`;
      for (const clas of obj.classes) {
        ret += `    <collection name="${clas.name}">\n`;
        for (const meth of clas.methods) {
          ret += `      ` +
            `<test name="${meth.name}" type="${obj.name}.${clas.name}" method="${meth.name}" time="0" result="${meth.result}"></test>\n`;
        }
        ret += `    </collection>\n`;
      }
      ret += `  </assembly>\n`;
    }
    ret += `</assemblies>`;

    return ret;
  }
}


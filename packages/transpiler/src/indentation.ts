export class Indentation {

  // todo, add tests for this class
  public run(js: string): string {
    let i = 0;
    const output: string[] = [];

    for (const l of js.split("\n")) {
      if (l.startsWith("}")) {
        i = i - 1;
      }
      if (i > 0) {
        output.push(" ".repeat(i * 2) + l);
      } else {
        output.push(l);
      }
      if (l.endsWith(" {")) {
        i = i + 1;
      }
    }

    return output.join("\n");
  }

}
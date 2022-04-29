
export class ABAPRegExp {
  // converts from ABAP specific regex to javascript regex
  public static convert(input: string): string {
    let ret = input;
    ret = input.replace(/\[\[:punct:\]\]/g, "[\\.\\,]");
    return ret;
  }
}

export function expandDynamic(code: string) {
  if (code === "") {
    return "1 = 1";
  } else {
    return code;
  }
}
export function assert(input: boolean) {
  if (input === false) {
    throw new Error("ASSERTION_FAILED");
  }
}
// A reference has no value to order or hash by, only a target. Two references
// are equal when they point to the same target, and a key over a reference
// (a hashed table keyed by a REF TO data, a hashed secondary key over a REF TO
// component) needs an order and a hash the sort, the binary search and the
// lookup agree on: every target gets a number on first use, which stands for
// the rest of its life. The order is unspecified but stable, as on a system.
const referenceIds = new WeakMap<object, number>();
let nextReferenceId = 1;

/** the identity of a reference target; an unbound reference is 0 */
export function referenceIdentity(target: object | undefined | null): number {
  if (target === undefined || target === null) {
    return 0;
  }
  let id = referenceIds.get(target);
  if (id === undefined) {
    id = nextReferenceId++;
    referenceIds.set(target, id);
  }
  return id;
}

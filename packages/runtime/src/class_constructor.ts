/** Runs the class constructors of a class at its first use, as a system does: the class constructors
 *  of the super classes first, then the class's own, each once. A class with its own class constructor
 *  has a static INTERNAL_CCTOR_DONE field, false until it runs; a class without one reads the field of
 *  its super class through the prototype chain, which has the same meaning for it.
 *  The flag is set before the class constructor runs, so it can use its own class.
 *  cls is the JavaScript class, undefined or classes without class constructors do nothing */
export async function classConstructor(cls: any): Promise<void> {
  if (typeof cls !== "function") {
    return;
  }
  const parent = Object.getPrototypeOf(cls);
  if (typeof parent === "function" && parent.INTERNAL_CCTOR_DONE === false) {
    await classConstructor(parent);
  }
  if (Object.prototype.hasOwnProperty.call(cls, "INTERNAL_CCTOR_DONE") && cls.INTERNAL_CCTOR_DONE === false) {
    cls.INTERNAL_CCTOR_DONE = true;
    await cls.class_constructor();
  }
}

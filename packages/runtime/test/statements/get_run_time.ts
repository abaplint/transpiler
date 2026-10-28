import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../src";

describe("Statement GET RUN TIME", () => {
  const modulus = 2147483648;

  for (const clock of ["performance", "Date"]) {
    it(`wraps modulo 2^31 with ${clock}`, () => {
      const performanceDescriptor = Object.getOwnPropertyDescriptor(globalThis, "performance");
      const dateNow = Date.now;
      let elapsed = 0;
      try {
        Object.defineProperty(globalThis, "performance", {
          configurable: true,
          value: clock === "performance" ? {now: () => elapsed / 1000} : undefined,
        });
        Date.now = () => elapsed / 1000;
        const abap = new ABAP({console: new MemoryConsole()});
        const target = new abap.types.Integer();
        // Date.now has millisecond resolution; performance.now has fractions.
        const readings = clock === "performance"
          ? [0, modulus - 1, modulus, modulus + 1, modulus - 1, modulus + 2, 2 * modulus + 3]
          : [0, 2147483000, 2147484000, 2147483000, 2147485000, 4294968000];
        const expected = clock === "performance"
          ? [0, modulus - 1, 0, 1, 1, 2, 3]
          : [0, 2147483000, 352, 352, 1352, 704];
        let last = 0;
        for (let index = 0; index < readings.length; index++) {
          elapsed = readings[index];
          abap.statements.getRunTime(target);
          expect(target.get()).to.equal(expected[index]);
          last = Math.max(last, elapsed);
          expect(abap.context.runTime?.last).to.equal(last);
        }
      } finally {
        Date.now = dateNow;
        if (performanceDescriptor) {
          Object.defineProperty(globalThis, "performance", performanceDescriptor);
        } else {
          Reflect.deleteProperty(globalThis, "performance");
        }
      }
    });
  }
});

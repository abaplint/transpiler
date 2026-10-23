import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../src";

describe("Statement GET TIME STAMP", () => {

  // the clock is fixed per case: new Date() answers the given instant
  function withClock(iso: string, body: () => void) {
    const RealDate = Date;
    const fixed = RealDate.parse(iso);
    class FixedDate extends RealDate {
      public constructor(...args: unknown[]) {
        if (args.length === 0) {
          super(fixed);
        } else {
          // @ts-ignore
          super(...args);
        }
      }
      public static now() {
        return fixed;
      }
    }
    globalThis.Date = FixedDate as DateConstructor;
    try {
      body();
    } finally {
      globalThis.Date = RealDate;
    }
  }

  const cases: [string, string][] = [
    // a whole second with a millisecond of 0 went below the second: 10:33:99,9990784
    ["2026-10-14T10:34:00.000Z", "20261014103400.0000000"],
    ["2026-10-14T05:31:00.001Z", "20261014053100.0010000"],
    // inside the second the noise was invisible until a compare or a diff
    ["2026-10-14T05:33:57.123Z", "20261014053357.1230000"],
    // the last second of a day went to 23:59:60,0013312
    ["2026-10-13T23:59:59.999Z", "20261013235959.9990000"],
  ];

  for (const [clock, expected] of cases) {
    it(`timestampl is exact at ${clock}`, () => {
      withClock(clock, () => {
        const abap = new ABAP({console: new MemoryConsole()});
        const stamp = new abap.types.Packed({length: 11, decimals: 7});
        abap.statements.getTime({stamp, sy: abap.builtin.sy});
        expect(stamp.toFixed(7)).to.equal(expected);
      });
    });
  }

  it("timestamp (no decimals) keeps the 14 digits", () => {
    withClock("2026-10-14T10:34:00.000Z", () => {
      const abap = new ABAP({console: new MemoryConsole()});
      const stamp = new abap.types.Packed({length: 8, decimals: 0});
      abap.statements.getTime({stamp, sy: abap.builtin.sy});
      expect(stamp.get()).to.equal(20261014103400);
    });
  });

});

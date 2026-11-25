import {expect} from "chai";
import {ABAP} from "../src";

describe("Date arithmetic and the machine's time zone", () => {

  // Node applies a new TZ value at once; the result must be the same in every zone
  function inZone(zone: string, body: () => void) {
    const before = process.env.TZ;
    process.env.TZ = zone;
    try {
      body();
    } finally {
      if (before === undefined) {
        delete process.env.TZ;
      } else {
        process.env.TZ = before;
      }
    }
  }

  for (const zone of ["America/Bogota", "Pacific/Pago_Pago", "UTC", "Europe/Berlin", "Pacific/Kiritimati"]) {
    it("date + days gives the same date in " + zone, () => {
      inZone(zone, () => {
        const abap = new ABAP();
        const base = new abap.types.Date();
        base.set("20260916");
        const due = new abap.types.Date();
        due.set(base.getNumeric() + 45);
        expect(due.get()).to.equal("20261031");

        const newYear = new abap.types.Date();
        newYear.set("20261231");
        const next = new abap.types.Date();
        next.set(newYear.getNumeric() + 1);
        expect(next.get()).to.equal("20270101");
        expect(next.getNumeric() - base.getNumeric()).to.equal(107);
      });
    });
  }

});

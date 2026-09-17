import {expect} from "chai";
import {ABAP} from "../../src";

// the numeric built-ins hand back a JavaScript number, and a row of a float
// table appended from one used to be 0: the number was wrapped as an Integer
// before the row saw it, so APPEND sin( x ) TO tab filled a sine table with
// zeros
describe("APPEND a raw number", () => {
  const floatTable = (abap: any) => abap.types.TableFactory.construct(new abap.types.Float({qualifiedName: "F"}),
    {withHeader: false, keyType: "EMPTY", primaryKey: {name: "primary_key", type: "STANDARD", isUnique: false, keyFields: []}, secondary: []} as any);
  const integerTable = (abap: any) => abap.types.TableFactory.construct(new abap.types.Integer(),
    {withHeader: false, keyType: "EMPTY", primaryKey: {name: "primary_key", type: "STANDARD", isUnique: false, keyFields: []}, secondary: []} as any);

  it("keeps the fraction in a float table", () => {
    const abap: any = new ABAP();
    (globalThis as any).abap = abap;
    const tab = floatTable(abap);
    const half = new abap.types.Float();
    half.set(0.5);
    abap.statements.append({source: abap.builtin.sin({val: half}), target: tab});
    abap.statements.append({source: 0.25, target: tab});
    abap.statements.append({source: 2, target: tab});
    const rows = tab.array();
    expect(rows.length).to.equal(3);
    expect(rows[0].getRaw()).to.be.closeTo(Math.sin(0.5), 1e-12);
    expect(rows[1].getRaw()).to.equal(0.25);
    expect(rows[2].getRaw()).to.equal(2);
  });

  it("rounds into an integer table as a move would", () => {
    const abap: any = new ABAP();
    (globalThis as any).abap = abap;
    const tab = integerTable(abap);
    abap.statements.append({source: 3.7, target: tab});
    abap.statements.append({source: 2, target: tab});
    expect(tab.array().map((r: any) => r.get())).to.deep.equal([4, 2]);
  });
});

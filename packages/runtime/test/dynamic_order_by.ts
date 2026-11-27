import {expect} from "chai";
import {ABAP} from "../src";

describe("dynamicOrderBy, the text of a dynamic ORDER BY", () => {
  const abap = new ABAP();

  it("blank-separated columns are separated by commas", () => {
    expect(abap.dynamicOrderBy("s~so_id i~so_item_pos")).to.equal("s~so_id, i~so_item_pos");
  });

  it("a direction stays with the column before it", () => {
    expect(abap.dynamicOrderBy("a DESCENDING b")).to.equal("a DESCENDING, b");
    expect(abap.dynamicOrderBy(" a ASCENDING  b DESCENDING ")).to.equal("a ASCENDING, b DESCENDING");
  });

  it("an already comma-separated list keeps its columns", () => {
    expect(abap.dynamicOrderBy("a, b DESCENDING")).to.equal("a, b DESCENDING");
    expect(abap.dynamicOrderBy("a,b c")).to.equal("a, b, c");
  });

  it("NULLS FIRST and NULLS LAST stay with the column before them", () => {
    expect(abap.dynamicOrderBy("a NULLS FIRST")).to.equal("a NULLS FIRST");
    expect(abap.dynamicOrderBy("a DESCENDING NULLS LAST b")).to.equal("a DESCENDING NULLS LAST, b");
    expect(abap.dynamicOrderBy("a nulls first, b")).to.equal("a nulls first, b");
  });

  it("one column, PRIMARY KEY and a text that is no plain list pass unchanged", () => {
    expect(abap.dynamicOrderBy("a DESCENDING")).to.equal("a DESCENDING");
    expect(abap.dynamicOrderBy("PRIMARY KEY")).to.equal("PRIMARY KEY");
    expect(abap.dynamicOrderBy("upper( a ) b")).to.equal("upper( a ) b");
  });
});

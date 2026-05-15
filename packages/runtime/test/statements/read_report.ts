import {expect} from "chai";
import {ABAP, DB} from "../../src";

describe("Statement READ REPORT", () => {

  it("Reads source from REPOSRC", async () => {
    const abap = new ABAP();
    (globalThis as any).abap = abap;

    let actualSelect = "";
    const db: DB.DatabaseClient = {
      name: "test",
      connect: async () => undefined,
      disconnect: async () => undefined,
      execute: async () => undefined,
      beginTransaction: async () => undefined,
      commit: async () => undefined,
      rollback: async () => undefined,
      delete: async () => ({subrc: 4, dbcnt: 0}),
      update: async () => ({subrc: 4, dbcnt: 0}),
      insert: async () => ({subrc: 4, dbcnt: 0}),
      select: async (options) => {
        actualSelect = options.select;
        return {rows: [{data: "WRITE '1'.\nWRITE '2'."}]};
      },
      openCursor: async () => {
        throw new Error("not implemented");
      },
    };
    abap.context.databaseConnections["DEFAULT"] = db;

    const target = abap.types.TableFactory.construct(new abap.types.String()) as typeof abap.types.Table.prototype;
    await abap.statements.readReport(abap.CharacterFactory.get(4, "zfoo"), {into: target});

    expect(actualSelect).to.contain(`FROM "reposrc"`);
    expect(actualSelect).to.contain(`"progname" = 'ZFOO`);
    expect(target.array().map(row => row.get())).to.deep.equal(["WRITE '1'.", "WRITE '2'."]);
    expect(abap.builtin.sy.get().subrc.get()).to.equal(0);
  });

});

import {expect} from "chai";
import initSqlJs from "sql.js";

describe("Basic standalone database", () => {

  it("create table, insert, and select", async() => {
    const SQL = await initSqlJs();
    const db = new SQL.Database();
    db.run("CREATE TABLE hello (a int, b char);");
    db.run("INSERT INTO hello VALUES (0, 'hello');");
    const res = db.exec("SELECT * FROM hello");
    expect(res.values().next().value.values[0][1]).to.equal("hello");
  });

});
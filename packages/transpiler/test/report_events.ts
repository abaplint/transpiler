import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {ReportEvents} from "../src/report_events";

describe("Report events", () => {

  it("every event statement of abaplint has its place: in the order of the runtime, or it does not run", async () => {
    // abaplint's own list of the event statements, it is not part of its typed index
    // eslint-disable-next-line @typescript-eslint/no-require-imports, @typescript-eslint/no-var-requires
    const events: (new () => any)[] = require("@abaplint/core/build/src/stuff").SELECTION_EVENTS;
    expect(events.length).to.be.greaterThan(0);
    const missing = events.filter(e => ReportEvents.eventOf(new abaplint.Nodes.StatementNode(new e())) === undefined);
    expect(missing.map(e => e.name)).to.deep.equal([]);
  });

  it("other statements start no event block", async () => {
    expect(ReportEvents.eventOf(new abaplint.Nodes.StatementNode(new abaplint.Statements.Report()))).to.equal(undefined);
    expect(ReportEvents.eventOf(new abaplint.Nodes.StatementNode(new abaplint.Statements.Write()))).to.equal(undefined);
  });

});

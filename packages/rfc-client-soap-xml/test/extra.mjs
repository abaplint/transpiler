const {RFCSoapClient} = await import("../build/client.js");

export async function setup(abap, schemas, insert) {
  abap.context.RFCDestinations["MYTESTTEST"] = new RFCSoapClient("http://localhost:45000");
}

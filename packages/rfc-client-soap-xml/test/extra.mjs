const {RFCSoapClient} = await import("../build/client.js");

global.abap.context.RFCDestinations["MYTESTTEST"] = new RFCSoapClient("http://localhost:45000");

import express from "express";

const PORT = 50000;

const app = express();
app.disable("x-powered-by");
app.set("etag", false);
app.use(express.raw({type: "*/*"}));

app.all("*", async (req, res) => {
  res.setHeader("content-type", "text/xml");
  res.send(`<?xml version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
  <SOAP-ENV:Body>
    <RFCDEMO:CAT_PING.Response xmlns:RFCDEMO="urn:sap-com:document:sap:rfc:functions">
      <SYSINFO>
        <SYSTEM>A4H</SYSTEM>
        <MAND>001</MAND>
        <ASPRA>E</ASPRA>
        <SNAME>DEVELOPER</SNAME>
        <CATTOK></CATTOK>
        <SAPRL>754</SAPRL>
        <HOST>vhcala4h</HOST>
        <OPSYS>Linux</OPSYS>
        <DBSYS>HDB</DBSYS>
        <DATEX>22012022</DATEX>
        <DCPFM></DCPFM>
        <MSNAME2>vhcala4hci</MSNAME2>
      </SYSINFO>
    </RFCDEMO:CAT_PING.Response>
  </SOAP-ENV:Body>
</SOAP-ENV:Envelope>`);
});

app.listen(PORT);
console.log("Listening on port http://localhost:" + PORT);
import fetch from "cross-fetch";
import {XMLParser} from "fast-xml-parser";
import * as runtime from "@abaplint/runtime";

export type ClientOptions = {
  username: string,
  password: string,
};

export class Client implements runtime.RFC.RFCClient {
  private readonly url: string;
  private readonly options: ClientOptions | undefined;

  public constructor(url: string, options?: ClientOptions) {
    this.url = url;
    this.options = options;
  }

  public async call(name: string, input?: runtime.RFC.RFCCallInput) {
    const body = this.buildBody(name, input);

    let auth = {};
    if (this.options?.username && this.options.password) {
      auth = {"Authorization": "Basic " + Buffer.from(this.options.username + ":" + this.options.password).toString("base64")};
    }

    const res = await fetch(this.url, {
      method: "POST",
      body,
      headers: {
        "Content-Type": "text/xml",
        ...auth,
      },
    });

    // todo
    console.dir(res.status);

    const xml = await res.text();
    console.dir(xml);
    new XMLParser({parseTagValue: false, ignoreAttributes: true, trimValues: false}).parse(xml);
    // todo
  }

  private buildBody(name: string, input?: CallInput): string {
    // eslint-disable-next-line max-len
    let body = `<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:RFCDEMO="urn:sap-com:document:sap:rfc:functions">
  <SOAP-ENV:Body>
    <RFCDEMO:${name}>`;

    for (const e in input?.exporting || []) {
      console.dir(e); // todo
    }
    for (const t in input?.tables || []) {
      console.dir(t); // todo
    }
    for (const c in input?.changing || []) {
      console.dir(c); // todo
    }

    body += `    </RFCDEMO:${name}>
  </SOAP-ENV:Body>
</SOAP-ENV:Envelope>`;
    return body;
  }

}
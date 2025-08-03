/* eslint-disable @typescript-eslint/no-unsafe-function-type */
import {ABAPObject} from "./types";

export type ABAPEventReference = {
  EVENT_NAME: string;
  EVENT_CLASS: string;
};

type Handlers = {
  handlers: Function[];
  forObject: ABAPObject | "ALL";
}[];

export class ABAPEventing {
  private readonly registrations: {[className: string]: {[eventName: string]: Handlers}} = {};

  /**
   * activation: true to activate the event, false to deactivate
   */
  public setHandler(event: ABAPEventReference, methods: Function[], forObject: ABAPObject | "ALL", activation: boolean): any {
    if (activation === false) {
      throw new Error("ABAPEventing.setHandler: deactivation not supported");
    } else if (methods.length === 0) {
      throw new Error("ABAPEventing.setHandler: no methods provided");
    }


    const ref = new WeakRef(forObject);
  }

  public raiseEvent(event: ABAPEventReference, parameters?: object): any {
// todo
  }
}
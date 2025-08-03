import {ABAPObject} from "./types";

export type ABAPEventReference = {
  EVENT_NAME: string;
  EVENT_CLASS: string;
};

type HandlerMethod = (parameters?: any) => Promise<void>;

type Handlers = {
  handlers: HandlerMethod[];
  forObject: WeakRef<ABAPObject> | "ALL";
}[];

export class ABAPEventing {
  private readonly registrations: {[className: string]: {[eventName: string]: Handlers}} = {};

  public setHandler(event: ABAPEventReference, methods: HandlerMethod[], forObject: ABAPObject | "ALL", activation: boolean): any {
    if (activation === false) {
      // todo
      throw new Error("ABAPEventing.setHandler: deactivation not supported");
    } else if (methods.length === 0) {
      throw new Error("ABAPEventing.setHandler: no methods provided");
    }

    if (!this.registrations[event.EVENT_CLASS]) {
      this.registrations[event.EVENT_CLASS] = {};
    }
    if (!this.registrations[event.EVENT_CLASS][event.EVENT_NAME]) {
      this.registrations[event.EVENT_CLASS][event.EVENT_NAME] = [];
    }

    const ref = forObject === "ALL" ? "ALL" : new WeakRef(forObject);
    const handlers = this.registrations[event.EVENT_CLASS][event.EVENT_NAME];
    // todo: tackle duplicates
    handlers.push({
      handlers: methods,
      forObject: ref,
    });
  }

  public raiseEvent(event: ABAPEventReference, parameters?: object): any {
// todo
  }
}
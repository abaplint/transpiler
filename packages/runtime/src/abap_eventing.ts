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
      throw new Error("ABAPEventing.setHandler: deactivation not supported, todo");
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

  public async raiseEvent(event: ABAPEventReference, me: ABAPObject, _parameters?: object): Promise<void> {
    const handlers = this.registrations[event.EVENT_CLASS]?.[event.EVENT_NAME];
    if (handlers === undefined) {
      return;
    }

    for (const handler of handlers) {
      if (handler.forObject === "ALL") {
        for (const method of handler.handlers) {
          await method(_parameters);
        }
      } else if (handler.forObject.deref() === me) {
        for (const method of handler.handlers) {
          await method(_parameters);
        }
      }
    }
  }
}
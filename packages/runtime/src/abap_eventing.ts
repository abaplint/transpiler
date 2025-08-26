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
    if (methods.length === 0) {
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
    if (activation === true) {
      // todo: tackle duplicates
      handlers.push({
        handlers: methods,
        forObject: ref,
      });
    } else {
      if (methods.length > 1) {
        throw new Error("ABAPEventing.setHandler: deactivation of multiple methods not supported, todo");
      }
      // todo: comparing the functions via toString might give the wrong result
      const index = handlers.findIndex(handler => handler.forObject === ref && handler.handlers[0].toString() === methods[0].toString());
      if (index !== -1) {
        handlers.splice(index, 1);
      }
    }
  }

  // todo: cleanup of dead WeakRefs
  public async raiseEvent(event: ABAPEventReference, me: ABAPObject, parameters: object): Promise<void> {
    const handlers = this.registrations[event.EVENT_CLASS]?.[event.EVENT_NAME];
    if (handlers === undefined) {
      return;
    }

    for (const handler of handlers) {
      if (handler.forObject === "ALL") {
        for (const method of handler.handlers) {
          await method(parameters);
        }
      } else if (handler.forObject.deref() === me) {
        for (const method of handler.handlers) {
          await method(parameters);
        }
      }
    }
  }
}
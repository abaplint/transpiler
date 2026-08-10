import {ABAPObject} from "./types";

export type ABAPEventReference = {
  EVENT_NAME: string;
  EVENT_CLASS: string;
};

export type HandlerMethod = {
  method: (parameters?: any) => Promise<void>;
  receiver: object;
};

type Handlers = {
  handlers: HandlerMethod[];
  forObject: WeakRef<object> | "ALL";
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

    const target = forObject === "ALL" ? "ALL" : forObject.get();
    const handlers = this.registrations[event.EVENT_CLASS][event.EVENT_NAME];
    if (activation === true) {
      // todo: tackle duplicates
      handlers.push({
        handlers: methods,
        forObject: target === "ALL" ? "ALL" : new WeakRef(target),
      });
    } else {
      if (methods.length > 1) {
        throw new Error("ABAPEventing.setHandler: deactivation of multiple methods not supported, todo");
      }
      const index = handlers.findIndex(handler => {
        const sameObject = handler.forObject === "ALL"
          ? target === "ALL"
          : target !== "ALL" && handler.forObject.deref() === target;
        const sameMethod = handler.handlers[0].method === methods[0].method
          && handler.handlers[0].receiver === methods[0].receiver;
        return sameObject && sameMethod;
      });
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
          await method.method.call(method.receiver, parameters);
        }
      } else if (handler.forObject.deref() === me.get()) {
        for (const method of handler.handlers) {
          await method.method.call(method.receiver, parameters);
        }
      }
    }
  }
}

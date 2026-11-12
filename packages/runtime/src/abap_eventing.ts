import {ABAP} from ".";
import {ABAPObject} from "./types";

declare const abap: ABAP;

export type ABAPEventReference = {
  EVENT_NAME: string;
  EVENT_CLASS: string;
  /** set when the handler is declared FOR EVENT ... OF a subclass of the class that declares the event */
  FOR_CLASS?: string;
  STATIC?: boolean;
};

export type HandlerMethod = {
  method: (parameters?: any) => Promise<void>;
  receiver: object;
};

type Handlers = {
  handlers: HandlerMethod[];
  forObject: WeakRef<object> | "ALL";
  forClass?: string;
  staticEvent?: boolean;
}[];

export class ABAPEventing {
  private readonly registrations: {[className: string]: {[eventName: string]: Handlers}} = {};

  /** returns sy-subrc: 4 a handler was registered already, 8 a handler to take out was not registered */
  public setHandler(event: ABAPEventReference, methods: HandlerMethod[], forObject: ABAPObject | "ALL", activation: boolean): number {
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
    let subrc = 0;
    for (const method of methods) {
      const index = handlers.findIndex(handler => {
        const sameObject = handler.forObject === "ALL"
          ? target === "ALL"
          : target !== "ALL" && handler.forObject.deref() === target;
        const sameMethod = handler.handlers[0].method === method.method
          && handler.handlers[0].receiver === method.receiver;
        return sameObject && sameMethod;
      });
      if (activation === true) {
        if (index !== -1) {
          subrc = 4;
          continue;
        }
        handlers.push({
          handlers: [method],
          forObject: target === "ALL" ? "ALL" : new WeakRef(target),
          forClass: event.FOR_CLASS,
          staticEvent: event.STATIC,
        });
      } else if (index === -1) {
        subrc = 8;
      } else {
        handlers.splice(index, 1);
      }
    }
    return subrc;
  }

  /** a handler declared for a subclass reacts to a static event only when a method of that class or a subclass of it
   * raises the event, and with FOR ALL INSTANCES to an instance event only of objects of that class */
  private reacts(handler: Handlers[number], me: ABAPObject, raisedIn: any): boolean {
    if (handler.forClass === undefined) {
      return true;
    }
    const cls = abap.Classes[handler.forClass];
    if (typeof cls !== "function") {
      return true;
    }
    if (handler.staticEvent === true) {
      return raisedIn === cls || raisedIn?.prototype instanceof cls;
    }
    return me.get() instanceof cls;
  }

  // todo: cleanup of dead WeakRefs
  public async raiseEvent(event: ABAPEventReference, me: ABAPObject, parameters: object, raisedIn?: any): Promise<void> {
    const handlers = this.registrations[event.EVENT_CLASS]?.[event.EVENT_NAME];
    if (handlers === undefined) {
      return;
    }

    for (const handler of handlers) {
      if (handler.forObject === "ALL") {
        if (this.reacts(handler, me, raisedIn) === false) {
          continue;
        }
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

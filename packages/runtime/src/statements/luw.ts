import {ABAP} from "..";

declare const abap: ABAP;

type Registered = {key: string, level: number, call: () => Promise<void>};

/** The SAP LUW of the running program: subroutines registered with PERFORM ... ON COMMIT | ON ROLLBACK and update
 *  function modules registered with CALL FUNCTION ... IN UPDATE TASK (when no host takes them) wait here. COMMIT WORK
 *  runs the ON COMMIT subroutines, by LEVEL and then in the order of registration, each once however often it was
 *  registered, then the update function modules in the order of registration; ROLLBACK WORK runs the ON ROLLBACK
 *  subroutines and forgets everything else. Either ends the LUW, and a new one starts. */
export class LUW {
  private onCommit: Registered[] = [];
  private onRollback: Registered[] = [];
  private updates: {name: string, param: any}[] = [];
  private local = false;

  public register(event: "COMMIT" | "ROLLBACK", key: string, call: () => Promise<void>, level = 0) {
    const list = event === "COMMIT" ? this.onCommit : this.onRollback;
    if (list.some(r => r.key === key)) {
      return;
    }
    list.push({key, level, call});
  }

  public registerUpdate(name: string, param: any) {
    this.updates.push({name, param});
  }

  /** SET UPDATE TASK LOCAL: sy-subrc 1 when an update function module is already registered for the normal update */
  public setLocal(): number {
    if (this.updates.length > 0 && this.local === false) {
      return 1;
    }
    this.local = true;
    return 0;
  }

  public async commit() {
    const forms = this.onCommit.map((r, index) => ({...r, index}))
      .sort((a, b) => a.level - b.level || a.index - b.index);
    const updates = this.updates;
    this.reset();
    for (const f of forms) {
      await f.call();
    }
    for (const u of updates) {
      const fm = abap.FunctionModules[u.name];
      await fm(u.param);
    }
  }

  public async rollback() {
    const forms = this.onRollback;
    this.reset();
    for (const f of forms) {
      await f.call();
    }
  }

  private reset() {
    this.onCommit = [];
    this.onRollback = [];
    this.updates = [];
    this.local = false;
  }
}

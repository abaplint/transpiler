export class ClassConstructors {
  private registered: (() => Promise<void>)[] = [];

  public register(func: () => Promise<void>) {
    this.registered.push(func);
  }

  public async execute(func: () => Promise<void>) {
    await func();
  }

  public async executeRegistered() {
    for (const r of this.registered) {
      await r();
    }
    this.registered = [];
  }

}
export class ClassConstructors {
  private readonly registered: (() => Promise<void>)[] = [];

  public register(func: () => Promise<void>) {
    this.registered.push(func);
  }

  public async execute() {
    for (const r of this.registered) {
      await r();
    }
  }

}
export class ABAPEvent extends Event {
  public parameters: object;

  public constructor(name: string, parameters: object ) {
    super(name);
    this.parameters = parameters;
  }
}
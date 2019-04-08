interface Array {

}

declare namespace isc {

  class Foo {

    Bar1(): void

    Bar2(): void

  }

  function getKeys(object: object): Array<any>;

  class SomeClass {

    someFun(bar: Bar1): void;

  }

}
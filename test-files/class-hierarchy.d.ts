declare namespace isc {

  class UberFoo {
    foo(x: string): void;
  }

  interface UberBar {
    bar(y: string): void;
  }

  class Foo extends UberFoo implements UberBar {
    foo(x: string): void;
    bar(y: string): void;
  }
}
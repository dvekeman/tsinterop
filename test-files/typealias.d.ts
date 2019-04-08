declare namespace isc {

  type Foo = string | string

  type Bar = string | number

  type FooBar = Foo

  /**
   * An operator is used as part of a Criterion when specifying AdvancedCriteria.
   *
   *  This list of operators indicates the set of operators built into SmartClient DataSources,
   *  which can be used for both client and server-side filtering.  You can extend the list of
   *  operators with DataSource.addSearchOperator.
   */
  type OperatorId =
    /**
     * exactly equal to
     */
    "equals" |

    /**
     * not equal to
     */
    "notEqual"

  class FooClass {

    fooMap: Map<FooBar>;

    foo: FooBar;

    foo(): Foo;

  }

}
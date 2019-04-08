@file:JsQualifier("isc")
package isc

/*
type Foo = String
*/
/*
type Bar = dynamic
*/
/*
type FooBar = Foo
*/
/*
type OperatorId = 
                  /**
                   *  exactly equal to
                   *  not equal to
                   */
                  String /* equals |  notEqual */
*/
external open class FooClass {
    open var fooMap: Map<String> = definedExternally
    open var foo: String = definedExternally
    open fun foo(): String = definedExternally
}

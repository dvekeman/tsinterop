@file:JsQualifier("isc")
external interface Array {
}
package isc

external open class Foo {
}
external fun getKeys(`object`: Any): Array<Any> = definedExternally

external open class SomeClass {
    open fun someFun(bar: (() -> Unit)): Unit = definedExternally
}

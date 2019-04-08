@file:JsQualifier("isc")
package isc

external open class UberFoo {
    open fun foo(x: String): Unit = definedExternally
}
external interface UberBar {
    fun bar (y: String): Unit
}
external open class Foo : UberFoo, UberBar {
    override fun foo(x: String): Unit = definedExternally
    override fun bar(y: String): Unit = definedExternally
}

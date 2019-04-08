@file:JsQualifier("isc")
package isc

external interface Palette {
    var defaultEditContext: dynamic
    var generateNames: Boolean
    fun setDefaultEditContext (defaultEditContext: dynamic): Unit
    fun makeEditNode (paletteNode: PaletteNode): EditNode
    fun foo (bar: String): Number
}
external open class ListGrid : Palette {
    open var generateNames: Boolean = definedExternally
    override fun makeEditNode(paletteNode: PaletteNode): EditNode = definedExternally
    open fun foo(): Number = definedExternally
}

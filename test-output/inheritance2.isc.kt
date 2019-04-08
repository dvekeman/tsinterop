@file:JsQualifier("isc")
package isc

/*
type HTMLString = String
*/
external open class FormItem {
    open var title: String = definedExternally
}
external open class SelectItem : FormItem {
    override var title: String = definedExternally
}

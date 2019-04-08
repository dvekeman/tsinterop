"--------------------------------------------------------------------------------"
"TypeScripts Declarations >>"
Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientClassDeclaration (Left (3,3)) "FormItem" Nothing Nothing Nothing [(Left (4,5),AmbientMemberDeclaration Nothing Nothing "f" (Left (Just (TypeReference (TypeRef (TypeName Nothing "Function") (Just [TypeReference (TypeRef (TypeName Nothing "String") Nothing),Predefined (Left (4,25)) (NumberType Nothing)]))))))]])]
"<< TypeScripts Declarations"
"--------------------------------------------------------------------------------"
"--------------------------------------------------------------------------------"
"Kotlin Declarations >>"
Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientClassDeclaration (Left (3,3)) "FormItem" Nothing Nothing Nothing [(Left (4,5),AmbientMemberDeclaration False Nothing Nothing "f" (Left (Just (FunctionType Nothing [] (Predefined (Left (0,0)) AnyType)))))]])]
"<< Kotlin Declarations"
"--------------------------------------------------------------------------------"
@file:JsQualifier("isc")
package isc

external open class FormItem {
    open var f: (Function<*>) = definedExternally
}

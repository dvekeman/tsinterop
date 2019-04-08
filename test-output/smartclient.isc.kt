"--------------------------------------------------------------------------------"
"TypeScripts Declarations >>"
Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientClassDeclaration (Left (3,3)) "Foo" Nothing Nothing Nothing [(Left (5,5),AmbientMemberDeclaration Nothing Nothing "Bar1" (Right (ParameterListAndReturnType Nothing [] (Just (Predefined (Left (5,13)) VoidType))))),(Left (7,5),AmbientMemberDeclaration Nothing Nothing "Bar2" (Right (ParameterListAndReturnType Nothing [] (Just (Predefined (Left (7,13)) VoidType)))))],AmbientClassDeclaration (Left (11,3)) "SomeClass" Nothing Nothing Nothing [(Left (13,5),AmbientMemberDeclaration Nothing Nothing "someFun" (Right (ParameterListAndReturnType Nothing [RequiredOrOptionalParameter Nothing "bar" Nothing (Just (ParameterType (TypeReference (TypeRef (TypeName Nothing "Bar1") Nothing))))] (Just (Predefined (Left (13,25)) VoidType)))))]])]
"<< TypeScripts Declarations"
"--------------------------------------------------------------------------------"
"--------------------------------------------------------------------------------"
"Kotlin Declarations >>"
Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientClassDeclaration (Left (3,3)) "Foo" Nothing Nothing Nothing [(Left (5,5),AmbientMemberDeclaration False Nothing Nothing "Bar1" (Right (ParameterListAndReturnType Nothing [] (Just (Predefined (Left (5,13)) VoidType))))),(Left (7,5),AmbientMemberDeclaration False Nothing Nothing "Bar2" (Right (ParameterListAndReturnType Nothing [] (Just (Predefined (Left (7,13)) VoidType)))))],AmbientClassDeclaration (Left (11,3)) "SomeClass" Nothing Nothing Nothing [(Left (13,5),AmbientMemberDeclaration False Nothing Nothing "someFun" (Right (ParameterListAndReturnType Nothing [RequiredOrOptionalParameter Nothing "bar" Nothing (Just (ParameterType (TypeReference (TypeRef (TypeName Nothing "Bar1") Nothing))))] (Just (Predefined (Left (13,25)) VoidType)))))]])]
"<< Kotlin Declarations"
"--------------------------------------------------------------------------------"
@file:JsQualifier("isc")
package isc

external open class Foo {
    open fun Bar1(): Unit = definedExternally
    open fun Bar2(): Unit = definedExternally
}
external open class SomeClass {
    open fun someFun(bar: Bar1): Unit = definedExternally
}

"--------------------------------------------------------------------------------"
"TypeScripts Declarations (Normalized) >>"
Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientTypeAliasDeclaration (TypeAlias (Left (3,3)) "HTMLString" (Predefined (Left (3,21)) (StringType Nothing))),AmbientClassDeclaration (Left (5,3)) "RPCRequest" Nothing Nothing Nothing [],AmbientClassDeclaration (Left (7,3)) "DSRequest" Nothing (Just [TypeRef (TypeName Nothing "RPCRequest") Nothing]) Nothing [],AmbientClassDeclaration (Left (9,3)) "FormItem" Nothing Nothing Nothing [(Left (10,5),AmbientMemberDeclaration Nothing Nothing "title" (Left (Just (Predefined (Left (3,21)) (StringType Nothing))))),(Left (12,5),AmbientMemberDeclaration Nothing Nothing "someValue" (Left (Just (Predefined (Left (12,16)) AnyType)))),(Left (14,5),AmbientMemberDeclaration Nothing Nothing "optionFilterContext" (Left (Just (TypeReference (TypeRef (TypeName Nothing "RPCRequest") Nothing)))))],AmbientClassDeclaration (Left (17,3)) "SelectItem" Nothing (Just [TypeRef (TypeName Nothing "FormItem") Nothing]) Nothing [(Left (18,5),AmbientMemberDeclaration Nothing Nothing "title" (Left (Just (Predefined (Left (3,21)) (StringType Nothing))))),(Left (20,5),AmbientMemberDeclaration Nothing Nothing "someValue" (Left (Just (Predefined (Left (20,16)) (StringType Nothing))))),(Left (22,5),AmbientMemberDeclaration Nothing Nothing "optionFilterContext" (Left (Just (TypeReference (TypeRef (TypeName Nothing "DSRequest") Nothing)))))]])]
"<< TypeScripts Declarations (Normalized)"
"--------------------------------------------------------------------------------"
"--------------------------------------------------------------------------------"
"Kotlin Declarations >>"
Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientTypeAliasDeclaration (TypeAlias (Left (3,3)) "HTMLString" (Predefined (Left (3,21)) (StringType Nothing))),AmbientClassDeclaration (Left (5,3)) "RPCRequest" Nothing Nothing Nothing [],AmbientClassDeclaration (Left (7,3)) "DSRequest" Nothing (Just [TypeRef (TypeName Nothing "RPCRequest") Nothing]) Nothing [],AmbientClassDeclaration (Left (9,3)) "FormItem" Nothing Nothing Nothing [(Left (10,5),AmbientMemberDeclaration False Nothing Nothing "title" (Left (Just (Predefined (Left (3,21)) (StringType Nothing))))),(Left (12,5),AmbientMemberDeclaration False Nothing Nothing "someValue" (Left (Just (Predefined (Left (12,16)) AnyType)))),(Left (14,5),AmbientMemberDeclaration False Nothing Nothing "optionFilterContext" (Left (Just (TypeReference (TypeRef (TypeName Nothing "RPCRequest") Nothing)))))],AmbientClassDeclaration (Left (17,3)) "SelectItem" Nothing (Just [TypeRef (TypeName Nothing "FormItem") Nothing]) Nothing [(Left (18,5),AmbientMemberDeclaration False Nothing Nothing "title" (Left (Just (Predefined (Left (3,21)) (StringType Nothing))))),(Left (20,5),AmbientMemberDeclaration True Nothing Nothing "someValue" (Left (Just (Predefined (Left (20,16)) (StringType Nothing))))),(Left (22,5),AmbientMemberDeclaration False Nothing Nothing "optionFilterContext" (Left (Just (TypeReference (TypeRef (TypeName Nothing "DSRequest") Nothing)))))]])]
"<< Kotlin Declarations"
"--------------------------------------------------------------------------------"
@file:JsQualifier("isc")
package isc

/*
type HTMLString = String
*/
external open class RPCRequest {
}
external open class DSRequest : RPCRequest {
}
external open class FormItem {
    open var title: String = definedExternally
    open var someValue: dynamic = definedExternally
    open var optionFilterContext: RPCRequest = definedExternally
}
external open class SelectItem : FormItem {
    open var title: String = definedExternally
    override var someValue: String = definedExternally
    open var optionFilterContext: DSRequest = definedExternally
}

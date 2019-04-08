"--------------------------------------------------------------------------------"
"TypeScripts Declarations >>"
Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientClassDeclaration (Left (3,3)) "FormItem" Nothing Nothing Nothing [(Left (4,5),AmbientMemberDeclaration Nothing Nothing "f" (Right (ParameterListAndReturnType Nothing [] (Just (TypeReference (TypeRef (TypeName Nothing "String") Nothing)))))),(Left (5,5),AmbientMemberDeclaration Nothing Nothing "f" (Right (ParameterListAndReturnType Nothing [RequiredOrOptionalParameter Nothing "foo" (Just Optional) (Just (ParameterType (Predefined (Left (5,13)) (NumberType Nothing)))),RequiredOrOptionalParameter Nothing "bar" Nothing (Just (ParameterType (Predefined (Left (5,26)) (StringType Nothing))))] (Just (TypeReference (TypeRef (TypeName Nothing "String") Nothing))))))]])]
"<< TypeScripts Declarations"
"--------------------------------------------------------------------------------"
"--------------------------------------------------------------------------------"
"TypeScripts Declarations (Normalized) >>"
Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientClassDeclaration (Left (3,3)) "FormItem" Nothing Nothing Nothing [(Left (4,5),AmbientMemberDeclaration Nothing Nothing "f" (Right (ParameterListAndReturnType Nothing [] (Just (TypeReference (TypeRef (TypeName Nothing "String") Nothing)))))),(Left (5,5),AmbientMemberDeclaration Nothing Nothing "f" (Right (ParameterListAndReturnType Nothing [RequiredOrOptionalParameter Nothing "foo" (Just Optional) (Just (ParameterType (Predefined (Left (5,13)) (NumberType Nothing)))),RequiredOrOptionalParameter Nothing "bar" Nothing (Just (ParameterType (Predefined (Left (5,26)) (StringType Nothing))))] (Just (TypeReference (TypeRef (TypeName Nothing "String") Nothing))))))]])]
"<< TypeScripts Declarations (Normalized)"
"--------------------------------------------------------------------------------"
"--------------------------------------------------------------------------------"
"Kotlin Declarations >>"
Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientClassDeclaration (Left (3,3)) "FormItem" Nothing Nothing Nothing [(Left (4,5),AmbientMemberDeclaration False Nothing Nothing "f" (Right (ParameterListAndReturnType Nothing [] (Just (TypeReference (TypeRef (TypeName Nothing "String") Nothing)))))),(Left (5,5),AmbientMemberDeclaration False Nothing Nothing "f" (Right (ParameterListAndReturnType Nothing [RequiredOrOptionalParameter Nothing "foo" (Just Optional) (Just (ParameterType (Predefined (Left (5,13)) (NumberType Nothing)))),RequiredOrOptionalParameter Nothing "bar" Nothing (Just (ParameterType (Predefined (Left (5,26)) (StringType Nothing))))] (Just (TypeReference (TypeRef (TypeName Nothing "String") Nothing))))))]])]
"<< Kotlin Declarations"
"--------------------------------------------------------------------------------"
@file:JsQualifier("isc")
package isc

open external class FormItem {
    open fun f(): String = definedExternally
    open fun f(foo: Number?, bar: String): String = definedExternally
}

Right [AmbientDeclaration (Left (1,1)) Nothing (AmbientNamespaceDeclaration (Left (1,9)) ["isc"] [AmbientClassDeclaration (Left (2,3)) "Blah" Nothing Nothing Nothing [(Left (3,5),AmbientMemberDeclaration Nothing Nothing "topOperatorOptions" (Left (Just (TypeReference (TypeRef (TypeName Nothing "Array") (Just [TypeReference (TypeRef (TypeName Nothing "Partial") (Just [TypeReference (TypeRef (TypeName Nothing "OperatorId") Nothing)]))]))))))]])]
@file:JsQualifier("isc")
package isc

external open class Blah {
    open var topOperatorOptions: Array<Any> = definedExternally
}

#!/usr/bin/env stack
-- stack --resolver lts-13.15 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Foldable (forM_)
import           Data.List           (intersperse, isSuffixOf, intercalate, length)
import           Data.Maybe          (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Language.Kotlin as KT
import qualified Language.TypeScript as TS
import           Lib

import           Data.Char           (isUpper)
import           Debug.Trace
import           Text.PrettyPrint    (Doc, nest, parens, render, text, ($$),
                                      (<+>))
import Control.Monad (when)
import qualified Turtle
import Prelude hiding (FilePath)

import System.Directory.PathWalk (pathWalk)
import System.FilePath.Posix ( (</>) )

{-
Currently this file is experimentally while getting some familiarity with TypeScript definition files.

Things to consider:

(1) Type Queries

```
// createBrowserHistory.d.ts
import { History } from './index';
import { getConfirmation } from './DOMUtils';

export interface BrowserHistoryBuildOptions {
  ...
  getUserConfirmation?: typeof getConfirmation;
  ...
}

// DOMUtils.d.ts
export function getConfirmation(message: string, callback: (result: boolean) => void): void;
```

Either read and parse *all* input files => 1 big AST
Or parse ASTs on demand (using the import statements)

(2) Output format

- Either keep the existing syntax (if the TS files already have a certain structure)
- Or output files per main declaration element (if the TS file is just one big bunch of definitions, e.g. smartclient)

(3) TODO: Union parameter types: create overloads???

-}

parser :: Turtle.Parser (Turtle.FilePath, Turtle.FilePath)
parser = (,) <$> Turtle.optPath "src"  's' "The source directory"
             <*> Turtle.optPath "dest" 'd' "The destination directory"

main :: IO ()
main = do
  (src, dest) <- Turtle.options "TypeScript to Kotlin definition file converstion" parser
  doMain (T.unpack $ Turtle.format Turtle.fp src) (T.unpack $ Turtle.format Turtle.fp dest) "" False

--  doMain "/src/SLIMS63/slimsclientkt/typings/smartclient.d.ts" "isc" True
--  doMain "test-files/interface-simple.ts" "blah"
--  doMain "test-files/function-with-comment.d.ts" "blah"
--  doMain "test-files/namespace-with-function.d.ts" "blah"
--  doMain "test-files/namespace-with-function2.d.ts" "blah"
--  doMain "test-files/class.d.ts" "blah"
--  doMain "test-files/bigclass.d.ts" "blah"
--  doMain "test-files/namespace-with-type.d.ts" "blah"
--  doMain "test-files/partial.d.ts" "blah"
--  doMain "test-files/class-hierarchy.d.ts" "blah"
--  doMain "test-files/interface.d.ts" "blah"
--  doMain "test-files/function.d.ts" "blah"
--  doMain "test-files/typealias.d.ts" "blah"
--  doMain "test-files/innerclass.d.ts" "blah"
--  doMain "test-files/inheritance.d.ts" "blah"
--  doMain "test-files/inheritance2.d.ts" "blah"
--  doMain "test-files/union.d.ts" "blah"
--  doMain "test-files/function-optionals.d.ts" "blah"
doMain :: String -> String -> String -> Bool -> IO ()
doMain inputDir outputDir qualifier custom =
  pathWalk inputDir $ \root dirs files ->
    forM_ files $ \file ->
      when (".d.ts" `isSuffixOf` file) $ do
        tsDecls <- parseFromFile TS.declarationSourceFile (root </> file)
        let ctx = either (const emptyAmbientContext) mkAmbientContext tsDecls
            tsDecls' = fmap (tsNormalize ctx) <$> tsDecls
            ctx' = either (const emptyAmbientContext) mkAmbientContext tsDecls'
            ktDecls = fmap (ts2kt ctx') <$> tsDecls'
            eitherErrOrTypedef = KT.renderDeclarationSourceFile qualifier <$> ktDecls
        case eitherErrOrTypedef of
          Left err -> print err
          Right out ->
            let filename = take (length file - 5 {-".d.ts"-}) file
            in writeSourceFile (outputDir </> filename ++ ".kt", out)
        when custom $
          case ktDecls of
            Left err -> print err
            Right decls -> customWriteSourceFiles outputDir qualifier decls

--  let ctx = either (const initialContext) (\decls -> initialContext { declarations = decls }) tsDecls
--  print "--------------------------------------------------------------------------------"
--  print "TypeScripts Declarations >>"
--  print tsDecls
--  print "<< TypeScripts Declarations"
--  print "--------------------------------------------------------------------------------"
--
--  print "--------------------------------------------------------------------------------"
--  print "TypeScripts Declarations (Normalized) >>"
--  print tsDecls'
--  print "<< TypeScripts Declarations (Normalized)"
--  print "--------------------------------------------------------------------------------"
--
--  print "--------------------------------------------------------------------------------"
--  print "Kotlin Declarations >>"
--  print ktDecls
--  print "<< Kotlin Declarations"
--  print "--------------------------------------------------------------------------------"
-- Custom Rendering
customWriteSourceFiles :: String -> String -> [KT.DeclarationElement] -> IO ()
customWriteSourceFiles outPath qualifier els = mapM_ writeSourceFile $ customRenderDeclarationSourceFile qualifier outPath els

writeSourceFile :: (String, String) -> IO ()
writeSourceFile (path, content) = do
  writeFile path content
  putStrLn $ "Wrote " ++ path

customRenderDeclarationSourceFile :: String -> String -> [KT.DeclarationElement] -> [(String, String)]
customRenderDeclarationSourceFile outPath qualifier els =
  (\(name, doc) -> (outPath ++ "/" ++ name ++ "." ++ qualifier ++ ".kt", render doc)) <$> customDeclarationSourceFile qualifier els

customDeclarationSourceFile :: String -> [KT.DeclarationElement] -> [(String, Doc)]
customDeclarationSourceFile qualifier elements =
  let header =
        trace ("qualifier: " ++ qualifier) $
        if not $ null qualifier
        then text "@file:JsQualifier" <> parens (text "\"" <> text qualifier <> text "\"")
        else text ""
      customElements = customDeclarationElement =<< elements
   in (\(name, doc) -> (name, header $$ nest 0 doc)) <$> customElements

customDeclarationElement :: KT.DeclarationElement -> [(String, Doc)]
customDeclarationElement (KT.AmbientDeclaration _ _e a) = customRenderAmbientNamespace a
customDeclarationElement _ = []

customRenderAmbientNamespace :: KT.Ambient -> [(String, Doc)]
customRenderAmbientNamespace ambient@(KT.AmbientClassDeclaration _ name _ _ _ _) =
  [(name, KT.renderAmbientClassDeclaration ambient)]
customRenderAmbientNamespace ambient@(KT.AmbientInterfaceDeclaration (KT.Interface _ name _ _ _)) =
  [(name, KT.renderAmbientInterfaceDeclaration ambient)]
customRenderAmbientNamespace (KT.AmbientNamespaceDeclaration _ name ds) =
  let header = (text "package" <+> KT.sepBy KT.dot text name $$ text "")
      classes = customRenderAmbientNamespace =<< ds
   in (\(docName, doc) -> (docName, header $$ nest 0 doc)) <$> classes
customRenderAmbientNamespace _ = []

-- Normalization
tsNormalize :: AmbientContext -> TS.DeclarationElement -> TS.DeclarationElement
tsNormalize ctx (TS.InterfaceDeclaration comment exported interface) =
  TS.InterfaceDeclaration (tsCommentNormalize comment) (TS.Exported <$ exported) (tsInterfaceNormalize ctx interface)
tsNormalize ctx (TS.TypeAliasDeclaration comment exported typeAlias) =
  TS.TypeAliasDeclaration (tsCommentNormalize comment) (TS.Exported <$ exported) (tsTypeAliasNormalize ctx typeAlias)
tsNormalize _ctx (TS.ExportDeclaration name) = TS.ExportDeclaration name
tsNormalize ctx (TS.AmbientDeclaration comment exported ambient) =
  TS.AmbientDeclaration (tsCommentNormalize comment) (TS.Exported <$ exported) (tsAmbientNormalize ctx Nothing ambient)
tsNormalize _ctx (TS.Unsupported s) = TS.Unsupported s

tsTypeAliasNormalize :: AmbientContext -> TS.TypeAlias -> TS.TypeAlias
tsTypeAliasNormalize ctx (TS.TypeAlias comment name type_) =
  TS.TypeAlias (tsCommentNormalize comment) name (tsTypeNormalize ctx type_)

tsInterfaceNormalize :: AmbientContext -> TS.Interface -> TS.Interface
tsInterfaceNormalize ctx (TS.Interface comment name mTypeParameters mTypeRefs typeBody) =
  TS.Interface
    (tsCommentNormalize comment)
    name
    (fmap (tsTypeParameterNormalize ctx) <$> mTypeParameters)
    (fmap (tsTypeRefNormalize ctx) <$> mTypeRefs)
    (tsTypeBodyNormalize ctx typeBody)

tsTypeRefNormalize :: AmbientContext -> TS.TypeRef -> TS.TypeRef
tsTypeRefNormalize ctx (TS.TypeRef typeName mTypes) =
  TS.TypeRef (tsTypeNameNormalize typeName) (fmap (tsTypeNormalize ctx) <$> mTypes)

tsTypeBodyNormalize :: AmbientContext -> TS.TypeBody -> TS.TypeBody
tsTypeBodyNormalize ctx (TS.TypeBody typeMembers) =
  TS.TypeBody (fmap (\t -> (tsCommentNormalize $ fst t, tsTypeMemberNormalize ctx $ snd t)) typeMembers)

tsTypeMemberNormalize :: AmbientContext -> TS.TypeMember -> TS.TypeMember
tsTypeMemberNormalize ctx (TS.PropertySignature name mOptional mType) =
  TS.PropertySignature name mOptional (tsTypeNormalize ctx <$> mType)
tsTypeMemberNormalize ctx (TS.CallSignature parameterListAndReturnType) =
  TS.CallSignature (tsParameterListAndReturnTypeNormalize ctx parameterListAndReturnType)
tsTypeMemberNormalize ctx (TS.ConstructSignature mTypeParameters parameters mType) =
  TS.ConstructSignature
    (fmap (tsTypeParameterNormalize ctx) <$> mTypeParameters)
    (tsParameterNormalize ctx <$> parameters)
    (tsTypeNormalize ctx <$> mType)
tsTypeMemberNormalize ctx (TS.TypeIndexSignature indexSignature) =
  TS.TypeIndexSignature (tsIndexSignatureNormalize ctx indexSignature)
tsTypeMemberNormalize ctx (TS.MethodSignature name mOptional parameterListAndReturnType) =
  TS.MethodSignature name mOptional (tsParameterListAndReturnTypeNormalize ctx parameterListAndReturnType)

tsIndexSignatureNormalize :: AmbientContext -> TS.IndexSignature -> TS.IndexSignature
tsIndexSignatureNormalize ctx (TS.IndexSignature name stringOrNumber type_) =
  TS.IndexSignature name (tsStringOrNumberNormalize stringOrNumber) (tsTypeNormalize ctx type_)

tsStringOrNumberNormalize :: TS.StringOrNumber -> TS.StringOrNumber
tsStringOrNumberNormalize TS.String = TS.String
tsStringOrNumberNormalize TS.Number = TS.Number

tsTypeNameNormalize :: TS.TypeName -> TS.TypeName
tsTypeNameNormalize (TS.TypeName mModuleName name) = TS.TypeName (tsModuleNameNormalize <$> mModuleName) name

tsModuleNameNormalize :: TS.ModuleName -> TS.ModuleName
tsModuleNameNormalize (TS.ModuleName names) = TS.ModuleName names

tsCommentNormalize :: TS.CommentPlaceholder -> TS.CommentPlaceholder
tsCommentNormalize comment =
  case comment of
    Left v  -> Left v
    Right c -> Right $ TS.Comment (TS.commentText c) (TS.commentOther c)

tsAmbientNormalize :: AmbientContext -> Maybe TS.Ambient -> TS.Ambient -> TS.Ambient
tsAmbientNormalize ctx _mParent (TS.AmbientVariableDeclaration comment name mType) =
  TS.AmbientVariableDeclaration (tsCommentNormalize comment) name (tsTypeNormalize ctx <$> mType)
tsAmbientNormalize ctx _mParent (TS.AmbientFunctionDeclaration comment name parameterListAndReturnType) =
  TS.AmbientFunctionDeclaration
    (tsCommentNormalize comment)
    name
    (tsParameterListAndReturnTypeNormalize ctx parameterListAndReturnType)
tsAmbientNormalize ctx _mParent ambientClass@(TS.AmbientClassDeclaration comment name mTypeParameters mExtendsTypeRefs mImplementsTypeRefs ambientClassBodyElements) =
  let members = foldr (\(cmt, el) acc -> ((cmt, ) <$> ambientClassBodyMember el) ++ acc) [] ambientClassBodyElements
   in TS.AmbientClassDeclaration
        (tsCommentNormalize comment)
        name
        (fmap (tsTypeParameterNormalize ctx) <$> mTypeParameters)
        (fmap (tsTypeRefNormalize ctx) <$> mExtendsTypeRefs)
        (fmap (tsTypeRefNormalize ctx) <$> mImplementsTypeRefs)
        (mapTuples tsCommentNormalize (tsAmbientClassBodyElementNormalize ctx (Just ambientClass)) members)
tsAmbientNormalize ctx _mParent (TS.AmbientInterfaceDeclaration interface) =
  TS.AmbientInterfaceDeclaration (tsInterfaceNormalize ctx interface)
tsAmbientNormalize _ctx _mParent (TS.AmbientEnumDeclaration comment mConstEnum name enums) =
  TS.AmbientEnumDeclaration (tsCommentNormalize comment) (tsConstEnumNormalize <$> mConstEnum) name enums
tsAmbientNormalize ctx _mParent (TS.AmbientTypeAliasDeclaration typeAlias) =
  TS.AmbientTypeAliasDeclaration (tsTypeAliasNormalize ctx typeAlias)
tsAmbientNormalize ctx _mParent ambientModule@(TS.AmbientModuleDeclaration comment strings ambients) =
  TS.AmbientModuleDeclaration
    (tsCommentNormalize comment)
    strings
    (tsAmbientNormalize ctx (Just ambientModule) <$> ambients)
tsAmbientNormalize ctx _mParent ambientNamespace@(TS.AmbientNamespaceDeclaration comment strings ambients) =
  TS.AmbientNamespaceDeclaration
    (tsCommentNormalize comment)
    strings
    (tsAmbientNormalize ctx (Just ambientNamespace) <$> ambients)
tsAmbientNormalize ctx _mParent ambientExternalModule@(TS.AmbientExternalModuleDeclaration comment name ambientExternalModuleElements) =
  TS.AmbientExternalModuleDeclaration
    (tsCommentNormalize comment)
    name
    (tsAmbientExternalModuleElementNormalize ctx (Just ambientExternalModule) <$> ambientExternalModuleElements)
tsAmbientNormalize _ctx _mParent (TS.AmbientImportDeclaration comment name entityName) =
  TS.AmbientImportDeclaration (tsCommentNormalize comment) name (tsEntityNameNormalize entityName)
--tsAmbientNormalize _ctx _mParent (TS.AmbientPathImportDeclaration comment imports paths) =
--  TS.AmbientPathImportDeclaration (tsCommentNormalize comment) imports paths
tsAmbientNormalize _ctx _mParent (TS.AmbientExternalImportDeclaration comment name imp) =
  TS.AmbientExternalImportDeclaration (tsCommentNormalize comment) name imp

tsEntityNameNormalize :: TS.EntityName -> TS.EntityName
tsEntityNameNormalize (TS.EntityName mModuleName name) = TS.EntityName (tsModuleNameNormalize <$> mModuleName) name

tsConstEnumNormalize :: TS.ConstEnum -> TS.ConstEnum
tsConstEnumNormalize TS.ConstEnum = TS.ConstEnum

tsAmbientClassBodyElementNormalize ::
     AmbientContext -> Maybe TS.Ambient -> TS.AmbientClassBodyElement -> TS.AmbientClassBodyElement
tsAmbientClassBodyElementNormalize ctx _mParent (TS.AmbientConstructorDeclaration parameters) =
  TS.AmbientConstructorDeclaration (tsParameterNormalize ctx <$> parameters)
tsAmbientClassBodyElementNormalize ctx _mParent (TS.AmbientMemberDeclaration mPublicOrPrivate mStatic name eitherMTypeOrParameterListAndReturnType) =
  TS.AmbientMemberDeclaration
    mPublicOrPrivate
    (tsStaticNormalize <$> mStatic)
    name
    (mapEither
       (fmap (tsTypeNormalize ctx))
       (tsParameterListAndReturnTypeNormalize ctx)
       eitherMTypeOrParameterListAndReturnType)
tsAmbientClassBodyElementNormalize ctx _mParent (TS.AmbientIndexSignature indexSignature) =
  TS.AmbientIndexSignature (tsIndexSignatureNormalize ctx indexSignature)

tsStaticNormalize :: TS.Static -> TS.Static
tsStaticNormalize TS.Static = TS.Static

tsAmbientExternalModuleElementNormalize ::
     AmbientContext -> Maybe TS.Ambient -> TS.AmbientExternalModuleElement -> TS.AmbientExternalModuleElement
tsAmbientExternalModuleElementNormalize ctx mParent (TS.AmbientModuleElement ambient) =
  TS.AmbientModuleElement (tsAmbientNormalize ctx mParent ambient)
tsAmbientExternalModuleElementNormalize _ctx _mParent (TS.ExportAssignment name) = TS.ExportAssignment name

tsParameterListAndReturnTypeNormalize ::
     AmbientContext -> TS.ParameterListAndReturnType -> TS.ParameterListAndReturnType
tsParameterListAndReturnTypeNormalize ctx (TS.ParameterListAndReturnType mTypeParameters parameters mType) =
  TS.ParameterListAndReturnType
    (fmap (tsTypeParameterNormalize ctx) <$> mTypeParameters)
    (tsParameterNormalize ctx <$> parameters)
    (tsTypeNormalize ctx <$> mType)

tsTypeParameterNormalize :: AmbientContext -> TS.TypeParameter -> TS.TypeParameter
tsTypeParameterNormalize _ctx (TS.PartialTypeParameter _) =
  TS.TypeParameter "" (Just $ TS.Predefined (Left (0, 0)) TS.AnyType)
tsTypeParameterNormalize ctx (TS.TypeParameter name mType) =
  let maybeAnAliasType = mapAlias ctx name
   in case maybeAnAliasType of
        Just aliasType -> TS.TypeParameter name (Just $ tsTypeNormalize ctx aliasType)
        Nothing -> TS.TypeParameter name (tsTypeNormalize ctx <$> mType)

tsParameterNormalize :: AmbientContext -> TS.Parameter -> TS.Parameter
tsParameterNormalize ctx (TS.RequiredOrOptionalParameter mPublicOrPrivate name mOptional mParameterType) =
  TS.RequiredOrOptionalParameter mPublicOrPrivate name mOptional (tsParameterTypeNormalize ctx <$> mParameterType)
tsParameterNormalize ctx (TS.RestParameter name mType) = TS.RestParameter name (tsTypeNormalize ctx <$> mType)

tsTypeNormalize :: AmbientContext -> TS.Type -> TS.Type
tsTypeNormalize _ctx (TS.Predefined comment predefinedType) =
  TS.Predefined (tsCommentNormalize comment) (tsPredefinedTypeNormalize predefinedType)
tsTypeNormalize _ctx (TS.TypeReference (TS.TypeRef (TS.TypeName _ "Partial") _)) =
  TS.Predefined (Left (0, 0)) TS.AnyType
tsTypeNormalize _ctx (TS.TypeReference (TS.TypeRef (TS.TypeName _mModuleName "Function") _mTypes)) =
  TS.FunctionType Nothing [] (TS.Predefined (Left (0, 0)) TS.AnyType)
tsTypeNormalize ctx (TS.TypeReference typeRef@(TS.TypeRef (TS.TypeName _mModuleName name) _mTypes)) =
  let maybeAnAliasType = mapAlias ctx name
   in case maybeAnAliasType of
        Just alias -> tsTypeNormalize ctx alias
        Nothing    -> TS.TypeReference (tsTypeRefNormalize ctx typeRef)
tsTypeNormalize ctx (TS.ObjectType typeBody) = TS.ObjectType (tsTypeBodyNormalize ctx typeBody)
tsTypeNormalize ctx (TS.ArrayType type_) = TS.ArrayType (tsTypeNormalize ctx type_)
tsTypeNormalize ctx (TS.UnionType type1@(TS.Predefined _ TS.AnyType) _type2) = tsTypeNormalize ctx type1
tsTypeNormalize ctx (TS.UnionType _type1 type2@(TS.Predefined _ TS.AnyType)) = tsTypeNormalize ctx type2
tsTypeNormalize ctx (TS.UnionType type1 type2) = TS.UnionType (tsTypeNormalize ctx type1) (tsTypeNormalize ctx type2)
tsTypeNormalize ctx (TS.TupleType types) = TS.TupleType (tsTypeNormalize ctx <$> types)
tsTypeNormalize _ctx (TS.TypeQuery strings) = TS.TypeQuery strings
tsTypeNormalize ctx (TS.FunctionType mTypeParameters parameters type_) =
  TS.FunctionType
    (fmap (tsTypeParameterNormalize ctx) <$> mTypeParameters)
    (tsParameterNormalize ctx <$> parameters)
    (tsTypeNormalize ctx type_)
tsTypeNormalize ctx (TS.ConstructorType mTypeParameters parameters type_) =
  TS.ConstructorType
    (fmap (tsTypeParameterNormalize ctx) <$> mTypeParameters)
    (tsParameterNormalize ctx <$> parameters)
    (tsTypeNormalize ctx type_)

tsParameterTypeNormalize :: AmbientContext -> TS.ParameterType -> TS.ParameterType
tsParameterTypeNormalize ctx (TS.ParameterType type_) = TS.ParameterType (tsTypeNormalize ctx type_)
tsParameterTypeNormalize _ctx (TS.ParameterSpecialized string) = TS.ParameterSpecialized string

tsPredefinedTypeNormalize :: TS.PredefinedType -> TS.PredefinedType
tsPredefinedTypeNormalize TS.AnyType           = TS.AnyType
tsPredefinedTypeNormalize (TS.NumberType mVal) = TS.NumberType mVal
tsPredefinedTypeNormalize TS.BooleanType       = TS.BooleanType
tsPredefinedTypeNormalize (TS.StringType mVal) = TS.StringType mVal
tsPredefinedTypeNormalize TS.VoidType          = TS.VoidType
tsPredefinedTypeNormalize TS.NullType          = TS.NullType

-- TypeScript to Kotlin conversion
ts2kt :: AmbientContext -> TS.DeclarationElement -> KT.DeclarationElement
ts2kt ctx (TS.InterfaceDeclaration comment exported interface) =
  KT.InterfaceDeclaration (tsComment2Kt comment) (KT.Exported <$ exported) (tsInterface2Kt ctx interface)
ts2kt ctx (TS.TypeAliasDeclaration comment exported typeAlias) =
  KT.TypeAliasDeclaration (tsComment2Kt comment) (KT.Exported <$ exported) (tsTypeAlias2Kt ctx typeAlias)
ts2kt _ctx (TS.ExportDeclaration name) = KT.ExportDeclaration name
ts2kt ctx (TS.AmbientDeclaration comment exported ambient) =
  KT.AmbientDeclaration (tsComment2Kt comment) (KT.Exported <$ exported) (tsAmbient2Kt ctx Nothing ambient)
ts2kt _ctx (TS.Unsupported s) = KT.Unsupported s

tsTypeAlias2Kt :: AmbientContext -> TS.TypeAlias -> KT.TypeAlias
tsTypeAlias2Kt ctx (TS.TypeAlias comment name type_) = KT.TypeAlias (tsComment2Kt comment) name (tsType2Kt ctx type_)

tsInterface2Kt :: AmbientContext -> TS.Interface -> KT.Interface
tsInterface2Kt ctx (TS.Interface comment name mTypeParameters mTypeRefs typeBody) =
  KT.Interface
    (tsComment2Kt comment)
    name
    (fmap (tsTypeParameter2Kt ctx) <$> mTypeParameters)
    (fmap (tsTypeRef2Kt ctx) <$> mTypeRefs)
    (tsTypeBody2Kt ctx typeBody)

tsTypeRef2Kt :: AmbientContext -> TS.TypeRef -> KT.TypeRef
tsTypeRef2Kt ctx (TS.TypeRef typeName mTypes) = KT.TypeRef (tsTypeName2Kt typeName) (fmap (tsType2Kt ctx) <$> mTypes)

tsTypeBody2Kt :: AmbientContext -> TS.TypeBody -> KT.TypeBody
tsTypeBody2Kt ctx (TS.TypeBody typeMembers) =
  KT.TypeBody (fmap (\t -> (tsComment2Kt $ fst t, tsTypeMember2Kt ctx $ snd t)) typeMembers)

tsTypeMember2Kt :: AmbientContext -> TS.TypeMember -> KT.TypeMember
tsTypeMember2Kt ctx (TS.PropertySignature name mOptional mType) =
  KT.PropertySignature name (tsOptional2Kt <$> mOptional) (tsType2Kt ctx <$> mType)
tsTypeMember2Kt ctx (TS.CallSignature parameterListAndReturnType) =
  KT.CallSignature (tsParameterListAndReturnType2Kt ctx parameterListAndReturnType)
tsTypeMember2Kt ctx (TS.ConstructSignature mTypeParameters parameters mType) =
  KT.ConstructSignature
    (fmap (tsTypeParameter2Kt ctx) <$> mTypeParameters)
    (tsParameter2Kt ctx <$> parameters)
    (tsType2Kt ctx <$> mType)
tsTypeMember2Kt ctx (TS.TypeIndexSignature indexSignature) =
  KT.TypeIndexSignature (tsIndexSignature2Kt ctx indexSignature)
tsTypeMember2Kt ctx (TS.MethodSignature name mOptional parameterListAndReturnType) =
  KT.MethodSignature name (tsOptional2Kt <$> mOptional) (tsParameterListAndReturnType2Kt ctx parameterListAndReturnType)

tsIndexSignature2Kt :: AmbientContext -> TS.IndexSignature -> KT.IndexSignature
tsIndexSignature2Kt ctx (TS.IndexSignature name stringOrNumber type_) =
  KT.IndexSignature name (tsStringOrNumber2Kt stringOrNumber) (tsType2Kt ctx type_)

tsStringOrNumber2Kt :: TS.StringOrNumber -> KT.StringOrNumber
tsStringOrNumber2Kt TS.String = KT.String
tsStringOrNumber2Kt TS.Number = KT.Number

tsTypeName2Kt :: TS.TypeName -> KT.TypeName
tsTypeName2Kt (TS.TypeName mModuleName name) = KT.TypeName (tsModuleName2Kt <$> mModuleName) name

tsModuleName2Kt :: TS.ModuleName -> KT.ModuleName
tsModuleName2Kt (TS.ModuleName names) = KT.ModuleName names

tsComment2Kt :: TS.CommentPlaceholder -> KT.CommentPlaceholder
tsComment2Kt comment =
  case comment of
    Left v  -> Left v
    Right c -> Right $ KT.Comment (TS.commentText c) (TS.commentOther c)

tsAmbient2Kt :: AmbientContext -> Maybe TS.Ambient -> TS.Ambient -> KT.Ambient
tsAmbient2Kt ctx _mParent (TS.AmbientVariableDeclaration comment name mType) =
  KT.AmbientVariableDeclaration (tsComment2Kt comment) name (tsType2Kt ctx <$> mType)
tsAmbient2Kt ctx _mParent (TS.AmbientFunctionDeclaration comment name parameterListAndReturnType) =
  KT.AmbientFunctionDeclaration
    (tsComment2Kt comment)
    name
    (tsParameterListAndReturnType2Kt ctx parameterListAndReturnType)
tsAmbient2Kt ctx _mParent ambientClass@(TS.AmbientClassDeclaration comment name mTypeParameters mExtendsTypeRefs mImplementsTypeRefs ambientClassBodyElements) =
  KT.AmbientClassDeclaration
    (tsComment2Kt comment)
    name
    (fmap (tsTypeParameter2Kt ctx) <$> mTypeParameters)
    (fmap (tsTypeRef2Kt ctx) <$> mExtendsTypeRefs)
    (fmap (tsTypeRef2Kt ctx) <$> mImplementsTypeRefs)
    (mapTuples tsComment2Kt (tsAmbientClassBodyElement2Kt ctx (Just ambientClass)) ambientClassBodyElements)
tsAmbient2Kt ctx _mParent (TS.AmbientInterfaceDeclaration interface) =
  KT.AmbientInterfaceDeclaration (tsInterface2Kt ctx interface)
tsAmbient2Kt _ctx _mParent (TS.AmbientEnumDeclaration comment mConstEnum name enums) =
  KT.AmbientEnumDeclaration (tsComment2Kt comment) (tsConstEnum2Kt <$> mConstEnum) name enums
tsAmbient2Kt ctx _mParent (TS.AmbientTypeAliasDeclaration typeAlias) =
  KT.AmbientTypeAliasDeclaration (tsTypeAlias2Kt ctx typeAlias)
tsAmbient2Kt ctx _mParent ambientModule@(TS.AmbientModuleDeclaration comment strings ambients) =
  KT.AmbientModuleDeclaration (tsComment2Kt comment) strings (tsAmbient2Kt ctx (Just ambientModule) <$> ambients)
tsAmbient2Kt ctx _mParent ambientNamespace@(TS.AmbientNamespaceDeclaration comment strings ambients) =
  KT.AmbientNamespaceDeclaration (tsComment2Kt comment) strings (tsAmbient2Kt ctx (Just ambientNamespace) <$> ambients)
tsAmbient2Kt ctx _mParent ambientExternalModule@(TS.AmbientExternalModuleDeclaration comment name ambientExternalModuleElements) =
  KT.AmbientExternalModuleDeclaration
    (tsComment2Kt comment)
    name
    (tsAmbientExternalModuleElement2Kt ctx (Just ambientExternalModule) <$> ambientExternalModuleElements)
tsAmbient2Kt _ctx _mParent (TS.AmbientImportDeclaration comment name entityName) =
  KT.AmbientImportDeclaration (tsComment2Kt comment) name (tsEntityName2Kt entityName)
--tsAmbient2Kt _ctx _mParent (TS.AmbientPathImportDeclaration comment imports paths) =
--  KT.AmbientPathImportDeclaration (tsComment2Kt comment) imports paths
tsAmbient2Kt _ctx _mParent (TS.AmbientExternalImportDeclaration comment name imp) =
  KT.AmbientExternalImportDeclaration (tsComment2Kt comment) name imp

tsEntityName2Kt :: TS.EntityName -> KT.EntityName
tsEntityName2Kt (TS.EntityName mModuleName name) =
  KT.EntityName (tsModuleName2Kt <$> mModuleName) name

tsConstEnum2Kt :: TS.ConstEnum -> KT.ConstEnum
tsConstEnum2Kt TS.ConstEnum = KT.ConstEnum

tsAmbientClassBodyElement2Kt ::
     AmbientContext -> Maybe TS.Ambient -> TS.AmbientClassBodyElement -> KT.AmbientClassBodyElement
tsAmbientClassBodyElement2Kt ctx _mParent (TS.AmbientConstructorDeclaration parameters) =
  KT.AmbientConstructorDeclaration (tsParameter2Kt ctx <$> parameters)
tsAmbientClassBodyElement2Kt ctx mParent member@(TS.AmbientMemberDeclaration mPublicOrPrivate mStatic name eitherMTypeOrParameterListAndReturnType) =
  let (_siblingMembers, inheritedClassMembers, inheritedInterfaceMembers) =
        maybe ([], [], []) (ambientClassMembers ctx) mParent
      isOverride =
        overridesFromClass member inheritedClassMembers || overridesFromInterface member inheritedInterfaceMembers
   in KT.AmbientMemberDeclaration
        isOverride
        (tsPublicOrPrivate <$> mPublicOrPrivate)
        (tsStatic2Kt <$> mStatic)
        name
        (mapEither (fmap (tsType2Kt ctx)) (tsParameterListAndReturnType2Kt ctx) eitherMTypeOrParameterListAndReturnType)
--      isOverload =
--        overloadsFromClass member inheritedClassMembers
--        || overloadsFromInterface member inheritedInterfaceMembers
--      overloadedName =
--        if isOverload
--        then (name ++) <$> returnTypeName eitherMTypeOrParameterListAndReturnType
--        else Nothing
--  KT.AmbientMemberDeclaration isOverride (tsPublicOrPrivate <$> mPublicOrPrivate) (tsStatic2Kt <$> mStatic) name overloadedName (mapEither (fmap tsType2Kt) tsParameterListAndReturnType2Kt eitherMTypeOrParameterListAndReturnType)
tsAmbientClassBodyElement2Kt ctx _mParent (TS.AmbientIndexSignature indexSignature) =
  KT.AmbientIndexSignature (tsIndexSignature2Kt ctx indexSignature)

tsStatic2Kt :: TS.Static -> KT.Static
tsStatic2Kt TS.Static = KT.Static

tsAmbientExternalModuleElement2Kt ::
     AmbientContext -> Maybe TS.Ambient -> TS.AmbientExternalModuleElement -> KT.AmbientExternalModuleElement
tsAmbientExternalModuleElement2Kt ctx mParent (TS.AmbientModuleElement ambient) =
  KT.AmbientModuleElement (tsAmbient2Kt ctx mParent ambient)
tsAmbientExternalModuleElement2Kt _ctx _mParent (TS.ExportAssignment name) = KT.ExportAssignment name

tsParameterListAndReturnType2Kt :: AmbientContext -> TS.ParameterListAndReturnType -> KT.ParameterListAndReturnType
tsParameterListAndReturnType2Kt ctx (TS.ParameterListAndReturnType mTypeParameters parameters mType) =
  KT.ParameterListAndReturnType
    (fmap (tsTypeParameter2Kt ctx) <$> mTypeParameters)
    (tsParameter2Kt ctx <$> parameters)
    (tsType2Kt ctx <$> mType)

tsTypeParameter2Kt :: AmbientContext -> TS.TypeParameter -> KT.TypeParameter
tsTypeParameter2Kt _ctx (TS.PartialTypeParameter _) =
  KT.TypeParameter "" (Just $ KT.Predefined (Left (0, 0)) KT.AnyType)
tsTypeParameter2Kt ctx (TS.TypeParameter name mType) = KT.TypeParameter name (tsType2Kt ctx <$> mType)

tsParameter2Kt :: AmbientContext -> TS.Parameter -> KT.Parameter
tsParameter2Kt ctx (TS.RequiredOrOptionalParameter mPublicOrPrivate name mOptional mParameterType) =
  KT.RequiredOrOptionalParameter
    (tsPublicOrPrivate <$> mPublicOrPrivate)
    name
    (tsOptional2Kt <$> mOptional)
    (tsParameterType2Kt ctx <$> mParameterType)
tsParameter2Kt ctx (TS.RestParameter name mType) = KT.RestParameter name (tsType2Kt ctx <$> mType)

tsType2Kt :: AmbientContext -> TS.Type -> KT.Type
tsType2Kt _ctx (TS.Predefined comment predefinedType) =
  KT.Predefined (tsComment2Kt comment) (tsPredefinedType2Kt predefinedType)
tsType2Kt _ctx (TS.TypeReference (TS.TypeRef (TS.TypeName _ "Partial") _)) = KT.Predefined (Left (0, 0)) KT.AnyType
tsType2Kt _ctx (TS.TypeReference (TS.TypeRef (TS.TypeName _mModuleName "Function") _mTypes)) =
  KT.FunctionType Nothing [] (KT.Predefined (Left (0, 0)) KT.AnyType)
tsType2Kt ctx (TS.TypeReference typeRef@(TS.TypeRef (TS.TypeName _mModuleName _name) _mTypes)) =
  KT.TypeReference (tsTypeRef2Kt ctx typeRef)
tsType2Kt ctx (TS.ObjectType typeBody) = KT.ObjectType (tsTypeBody2Kt ctx typeBody)
tsType2Kt ctx (TS.ArrayType type_) = postProcess (KT.ArrayType (tsType2Kt ctx type_))
    -- Array<dynamic> => Array<Any>
  where
    postProcess (KT.ArrayType (KT.Predefined comment KT.DynamicType)) = KT.ArrayType (KT.Predefined comment KT.AnyType)
    postProcess array = array
tsType2Kt ctx unionType@(TS.UnionType _ _) =
  let unionTypes = fromUnionType unionType
   in case unionTypes of
        [] -> KT.DynamicAliasType Nothing
        _ ->
          let firstType = head unionTypes
           in if all (compareTypes firstType) unionTypes
                then tsType2Kt ctx $ joinUnionTypes unionTypes
                else KT.DynamicAliasType (Just (tsType2Kt ctx <$> unionTypes))
tsType2Kt ctx (TS.TupleType types) = KT.TupleType (tsType2Kt ctx <$> types)
tsType2Kt _ctx (TS.TypeQuery strings) =
  -- TODO: Convert the type to another type, e.g.
  --  export interface BrowserHistoryBuildOptions {
  --    getUserConfirmation?: typeof getConfirmation;
  --  }
  --  ...
  -- export function getConfirmation(message: string, callback: (result: boolean) => void): void;
  -- KT.TypeQuery strings
  KT.DynamicAliasType Nothing
tsType2Kt ctx (TS.FunctionType mTypeParameters parameters type_) =
  KT.FunctionType
    (fmap (tsTypeParameter2Kt ctx) <$> mTypeParameters)
    (tsParameter2Kt ctx <$> parameters)
    (tsType2Kt ctx type_)
tsType2Kt ctx (TS.ConstructorType mTypeParameters parameters type_) =
  KT.ConstructorType
    (fmap (tsTypeParameter2Kt ctx) <$> mTypeParameters)
    (tsParameter2Kt ctx <$> parameters)
    (tsType2Kt ctx type_)

tsParameterType2Kt :: AmbientContext -> TS.ParameterType -> KT.ParameterType
tsParameterType2Kt ctx (TS.ParameterType type_) = KT.ParameterType (tsType2Kt ctx type_)
tsParameterType2Kt _ctx (TS.ParameterSpecialized string) = KT.ParameterSpecialized string

tsOptional2Kt :: TS.Optional -> KT.Optional
tsOptional2Kt TS.Optional = KT.Optional

tsPredefinedType2Kt :: TS.PredefinedType -> KT.PredefinedType
tsPredefinedType2Kt TS.AnyType           = KT.AnyType
tsPredefinedType2Kt (TS.NumberType mVal) = KT.NumberType mVal
tsPredefinedType2Kt TS.BooleanType       = KT.BooleanType
tsPredefinedType2Kt (TS.StringType mVal) = KT.StringType mVal
tsPredefinedType2Kt TS.VoidType          = KT.VoidType
tsPredefinedType2Kt TS.NullType          = KT.DynamicType

-- HELPER
tsPublicOrPrivate :: TS.PublicOrPrivate -> KT.PublicOrPrivate
tsPublicOrPrivate TS.Public  = KT.Public
tsPublicOrPrivate TS.Private = KT.Private

mapTuples :: (a -> c) -> (b -> d) -> [(a, b)] -> [(c, d)]
mapTuples f g ts = (\(t1, t2) -> (f t1, g t2)) <$> ts

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither f g eitherAOrB =
  case eitherAOrB of
    Left a  -> Left (f a)
    Right b -> Right (g b)

fromUnionType :: TS.Type -> [TS.Type]
fromUnionType = fromUnionType_ []
  where
    fromUnionType_ :: [TS.Type] -> TS.Type -> [TS.Type]
    fromUnionType_ types (TS.UnionType t1 t2@(TS.UnionType _ _)) = t1 : fromUnionType_ types t2
    fromUnionType_ types (TS.UnionType t1 t2) = t1 : t2 : types
    fromUnionType_ types t = t : types

showTypeName :: TS.Type -> String
showTypeName (TS.Predefined _ predefinedType) = predefinedTypeName predefinedType
showTypeName (TS.TypeReference (TS.TypeRef name _)) = show name
showTypeName (TS.ObjectType _) = "object"
showTypeName (TS.ArrayType type_) = "Array<" ++ show (showTypeName type_) ++ ">"
showTypeName (TS.UnionType type1 type2) =
  if showTypeName type1 == showTypeName type2
    then showTypeName type1
    else "dynamic"
showTypeName (TS.TupleType _) = undefined
showTypeName (TS.TypeQuery _) = undefined
showTypeName TS.FunctionType {} = undefined
showTypeName TS.ConstructorType {} = undefined

predefinedTypeName :: TS.PredefinedType -> String
predefinedTypeName TS.AnyType        = "Any"
predefinedTypeName (TS.NumberType _) = "Number"
predefinedTypeName TS.BooleanType    = "Boolean"
predefinedTypeName (TS.StringType _) = "String"
predefinedTypeName TS.NullType       = "null"
predefinedTypeName TS.VoidType       = "Void"

joinUnionTypes :: [TS.Type] -> TS.Type
joinUnionTypes []           = undefined
joinUnionTypes [t1]         = t1
joinUnionTypes (t1:t2:rest) = joinUnionTypes (joinUnionTypes_ t1 t2 : rest)

joinUnionTypes_ :: TS.Type -> TS.Type -> TS.Type
joinUnionTypes_ (TS.Predefined comment TS.AnyType) _ = TS.Predefined comment TS.AnyType
joinUnionTypes_ _ (TS.Predefined comment TS.AnyType) = TS.Predefined comment TS.AnyType
joinUnionTypes_ (TS.Predefined comment1 (TS.StringType mTypes1)) (TS.Predefined comment2 (TS.StringType mTypes2)) =
  TS.Predefined
    (joinComments comment1 comment2)
    (TS.StringType $
     case fromMaybe [] mTypes1 ++ fromMaybe [] mTypes2 of
       []             -> Nothing
       stringTypeList -> Just stringTypeList)
joinUnionTypes_ (TS.Predefined comment1 type1) (TS.Predefined comment2 type2) =
  if predefinedTypeName type1 == predefinedTypeName type2
    then TS.Predefined (comment1 <> comment2) type1
    else error $
         "Types must be the same. '" ++
         show type1 ++ "' != '" ++ show type2 ++ "'" ++ "\n" ++ show comment1 ++ "\n" ++ show comment2
joinUnionTypes_ t1@(TS.Predefined _ _) (TS.UnionType t2 u) = joinUnionTypes_ t1 (joinUnionTypes_ t2 u)
joinUnionTypes_ t1@(TS.TypeReference (TS.TypeRef typeName1 _mTypes1)) (TS.TypeReference (TS.TypeRef typeName2 _mTypes2)) =
  if compareTypeNames typeName1 typeName2
    then t1
    else error $ "Types must be the same. '" ++ debugTypeName typeName1 ++ "' != '" ++ debugTypeName typeName2 ++ "'"
joinUnionTypes_ t1 t2 =
  if compareTypes t1 t2
    then t1
    else error $ "Joining '" ++ show t1 ++ "' and '" ++ show t2 ++ "' is not yet supported."

joinComments :: TS.CommentPlaceholder -> TS.CommentPlaceholder -> TS.CommentPlaceholder
joinComments (Right c1) (Right c2) = Right (c1 <> c2)
joinComments (Left c1) (Left _c2)  = Left c1
joinComments (Right c1) (Left _c2) = Right c1
joinComments (Left _c1) (Right c2) = Right c2

ambientInterfaceMembers :: AmbientContext -> TS.Ambient -> ([TS.TypeMember], [TS.TypeMember])
ambientInterfaceMembers ctx (TS.AmbientInterfaceDeclaration (TS.Interface _ _name _mTypeParams mExtendsTypeRefs (TS.TypeBody els))) =
  let ownMembers = catMaybes $ ambientInterfaceBodyMember . snd <$> els
      joinTupleLists (x, y) = x ++ y
      allTypeRefs = fromMaybe [] mExtendsTypeRefs
      inheritedMembers -- Starting from the 'extends' clause, find all functions in the parent declarations
        -- Flatten everything together
       =
        concat $
            -- We don't care where the inherited function comes from (parent vs parent of parent vs ...)
        joinTupleLists . ambientInterfaceMembers ctx <$> ambientClassParents ctx allTypeRefs
   in (ownMembers, inheritedMembers)
--      trace (
--        "INTERFACE " ++ _name
--        ++ "\n"
--        ++ "own members: " ++ show (ambientInterfaceBodyElementName <$> ownMembers)
--        ++ "\n"
--        ++ "inherited members" ++ show (ambientInterfaceBodyElementName <$> inheritedMembers)
--      )
ambientInterfaceMembers _ctx _ = ([], [])

ambientClassMembers ::
     AmbientContext -> TS.Ambient -> ([TS.AmbientClassBodyElement], [TS.AmbientClassBodyElement], [TS.TypeMember])
ambientClassMembers ctx (TS.AmbientClassDeclaration _ _name _ mExtendsTypeRefs mImplementsTypeRef els) =
  let ownMembers = ambientClassBodyMember . snd =<< els
      allTypeRefs = fromMaybe [] mExtendsTypeRefs ++ fromMaybe [] mImplementsTypeRef
      allParents = ambientClassParents ctx allTypeRefs
      combineAncestors = reduceTripleToTuple
      -- Starting from the 'extends' clause, find all functions in the parent declarations
      inheritedAmbientClassMembers =
        foldr
          (\(classInherits, interfaceInherits) (classInheritsAcc, interfaceInheritsAcc) ->
             (classInherits ++ classInheritsAcc, interfaceInherits ++ interfaceInheritsAcc))
          ([], [])
          -- We don't care where the inherited function comes from (parent vs parent of parent vs ...)
          (combineAncestors . ambientClassMembers ctx <$> allParents)
   in (ownMembers, fst inheritedAmbientClassMembers, snd inheritedAmbientClassMembers)
--      trace (
--        "CLASS " ++ _name
--        ++ "\n"
--        ++ "own members: " ++ show (ambientClassBodyElementName <$> ownMembers)
--        ++ "\n"
--        ++ "inherited class members" ++ show (ambientClassBodyElementName <$> fst inheritedAmbientClassMembers)
--        ++ "\n"
--        ++ "inherited interface members" ++ show (ambientInterfaceBodyElementName <$> snd inheritedAmbientClassMembers)
--      )
ambientClassMembers ctx ambientInterface@(TS.AmbientInterfaceDeclaration _) =
  let (parentMembers, grandParentMembers) = ambientInterfaceMembers ctx ambientInterface
   in ([], [], parentMembers ++ grandParentMembers)
ambientClassMembers _ _ = ([], [], [])

--ambientClassFunctions :: AmbientContext -> TS.Ambient -> ([TS.AmbientClassBodyElement], [TS.AmbientClassBodyElement])
--ambientClassFunctions ctx (TS.AmbientClassDeclaration _ _ _ mExtendsTypeRefs mImplementsTypeRef els) =
--  let ownFunctions = catMaybes $ ambientClassBodyFunction . snd <$> els
--      joinTupleLists (x, y) = x ++ y
--      inheritedFunctions = -- Starting from the 'extends' clause, find all functions in the parent declarations
--        -- Flatten everything together
--        concat . fromMaybe [] $
--          fmap
--            -- We don't care where the inherited function comes from (parent vs parent of parent vs ...)
--            (joinTupleLists . ambientClassFunctions ctx) .
--              ambientClassParents ctx
--                <$> ((++) <$> mExtendsTypeRefs <*> mImplementsTypeRef)
--   in (ownFunctions, inheritedFunctions)
--ambientClassFunctions _ _ = ([], [])
--
--ambientClassProperties :: AmbientContext -> TS.Ambient -> ([TS.AmbientClassBodyElement], [TS.AmbientClassBodyElement])
--ambientClassProperties ctx (TS.AmbientClassDeclaration _ _ _ mExtendsTypeRefs mImplementsTypeRef els) =
--  let ownProps = catMaybes $ ambientClassBodyProperty . snd <$> els
--      joinTupleLists (x, y) = x ++ y
--      inheritedProps = -- Starting from the 'extends' clause, find all properties in the parent declarations
--        -- Flatten everything together
--        concat . fromMaybe [] $
--          fmap
--            -- We don't care where the inherited property comes from (parent vs parent of parent vs ...)
--            (joinTupleLists . ambientClassProperties ctx) .
--              ambientClassParents ctx
--                <$> ((++) <$> mExtendsTypeRefs <*> mImplementsTypeRef)
--   in (ownProps, inheritedProps)
--ambientClassProperties _ _ = ([], [])
ambientClassBodyMember :: TS.AmbientClassBodyElement -> [TS.AmbientClassBodyElement]
ambientClassBodyMember member@TS.AmbientMemberDeclaration {} = [member]
--ambientClassBodyMember member@(TS.AmbientMemberDeclaration mPubPriv mStatic name (Right (TS.ParameterListAndReturnType mTypeParams params mType))) =
--  (\newParams -> TS.AmbientMemberDeclaration mPubPriv mStatic name (Right (TS.ParameterListAndReturnType mTypeParams newParams mType))) <$>
--    optionalPermutations isOptionalParameter params
ambientClassBodyMember member@TS.AmbientIndexSignature {} = [member]
ambientClassBodyMember _ = []

isOptionalParameter :: TS.Parameter -> Bool
isOptionalParameter (TS.RequiredOrOptionalParameter _mPubPriv _name (Just TS.Optional) _mParamType) = True
isOptionalParameter _ = False

optionalPermutations :: Show a => (a -> Bool) -> [a] -> [[a]]
optionalPermutations isOptional params =
  let perms = reverse <$> optionalPermutations_ [[]] isOptional params
   in perms

optionalPermutations_ :: [[a]] -> (a -> Bool) -> [a] -> [[a]]
optionalPermutations_ res _ [] = res
optionalPermutations_ res isOptional (p:ps)
  | isOptional p =
    let withP = (p :) <$> res
        withoutP = res
     in optionalPermutations_ (withP ++ withoutP) isOptional ps
  | otherwise =
    let withP = (p :) <$> res
     in optionalPermutations_ withP isOptional ps

ambientInterfaceBodyMember :: TS.TypeMember -> Maybe TS.TypeMember
ambientInterfaceBodyMember member@TS.MethodSignature {}     = Just member
ambientInterfaceBodyMember member@TS.PropertySignature {}   = Just member
ambientInterfaceBodyMember member@(TS.TypeIndexSignature _) = Just member
ambientInterfaceBodyMember member@(TS.CallSignature _)      = Just member
ambientInterfaceBodyMember member@TS.ConstructSignature {}  = Just member

--ambientInterfaceBodyMember _ = Nothing
--ambientClassBodyFunction :: TS.AmbientClassBodyElement -> Maybe TS.AmbientClassBodyElement
--ambientClassBodyFunction member@(TS.AmbientMemberDeclaration _ _ _name (Right _parameterListAndReturnType)) = Just member
--ambientClassBodyFunction _ = Nothing
--
--ambientClassBodyProperty :: TS.AmbientClassBodyElement -> Maybe TS.AmbientClassBodyElement
--ambientClassBodyProperty member@(TS.AmbientMemberDeclaration _ _ _name (Left _mType)) = Just member
--ambientClassBodyProperty _ = Nothing
ambientClassParents :: AmbientContext -> [TS.TypeRef] -> [TS.Ambient]
ambientClassParents ctx typeRefs = concat $ findAmbientByName ctx <$> typeRefs

findAmbientByName :: AmbientContext -> TS.TypeRef -> [TS.Ambient]
findAmbientByName ctx (TS.TypeRef tname _) = findAmbientByName_ tname (declarations ctx ++ inheritedDeclarations ctx)

findAmbientByName_ :: TS.TypeName -> [TS.DeclarationElement] -> [TS.Ambient]
findAmbientByName_ typeName els = concat $ ambientByNameMatch typeName <$> els

ambientByNameMatch :: TS.TypeName -> TS.DeclarationElement -> [TS.Ambient]
ambientByNameMatch typeName (TS.AmbientDeclaration _ _ ambient) = ambientByNameMatchAmbient_ typeName ambient
ambientByNameMatch _ _ = []

ambientByNameMatchAmbient :: TS.TypeName -> [TS.Ambient] -> [TS.Ambient]
ambientByNameMatchAmbient typeName ambients = concat $ ambientByNameMatchAmbient_ typeName <$> ambients

ambientByNameMatchAmbient_ :: TS.TypeName -> TS.Ambient -> [TS.Ambient]
ambientByNameMatchAmbient_ (TS.TypeName _mModuleName tname) ambientClass@(TS.AmbientClassDeclaration _ className _mTypeParams _mExtends _mImplements _elems) =
  [ambientClass | tname == className]
ambientByNameMatchAmbient_ (TS.TypeName _mModuleName tname) ambientInterface@(TS.AmbientInterfaceDeclaration (TS.Interface _ interfaceName _mTypeParams _mExtends _typeBody)) =
  [ambientInterface | tname == interfaceName]
ambientByNameMatchAmbient_ typeName (TS.AmbientNamespaceDeclaration _ _ ambients) =
  ambientByNameMatchAmbient typeName ambients
ambientByNameMatchAmbient_ _ _ = []

debugTypeRefName :: TS.TypeRef -> String
debugTypeRefName (TS.TypeRef tName _) = show tName

debugTypeName :: TS.TypeName -> String
debugTypeName (TS.TypeName Nothing name) = "TypeName::" ++ name
debugTypeName (TS.TypeName (Just (TS.ModuleName names)) name) =
  "TypeName::" ++ show (intersperse "." names) ++ "." ++ name

overridesFromClass :: TS.AmbientClassBodyElement -> [TS.AmbientClassBodyElement] -> Bool
overridesFromClass function@TS.AmbientMemberDeclaration {} inherited = any (overridesFromClass_ function) inherited
overridesFromClass _ _ = False

overridesFromClass_ :: TS.AmbientClassBodyElement -> TS.AmbientClassBodyElement -> Bool
overridesFromClass_ type1 inheritedType =
  ambientClassBodyElementName type1 == ambientClassBodyElementName inheritedType &&
  compareMaybes
    compareParameterListAndReturnType
    (ambientClassBodyElementParams type1)
    (ambientClassBodyElementParams inheritedType)

--  trace (
--    "overridesFromClass_:"
--    ++ show type1
--    ++ "\n"
--    ++ show inheritedType
--  )
overridesFromInterface :: TS.AmbientClassBodyElement -> [TS.TypeMember] -> Bool
overridesFromInterface function@TS.AmbientMemberDeclaration {} inherited =
  any (overridesFromInterface_ function) inherited
overridesFromInterface _ _ = False

overridesFromInterface_ :: TS.AmbientClassBodyElement -> TS.TypeMember -> Bool
overridesFromInterface_ (TS.AmbientMemberDeclaration _mPublicOrPrivate _mStatic name1 (Right parameterListAndReturnType1)) (TS.MethodSignature name2 _mOptional parameterListAndReturnType2) =
  name1 == name2 &&
  compareParameterListAndReturnType (Right parameterListAndReturnType1) (Right parameterListAndReturnType2)
overridesFromInterface_ (TS.AmbientMemberDeclaration _mPublicOrPrivate _mStatic name1 (Left mType1)) (TS.PropertySignature name2 _mOptional mType2) =
  name1 == name2 && compareMaybes compareTypes mType1 mType2
overridesFromInterface_ (TS.AmbientMemberDeclaration _mPublicOrPrivate _mStatic name1 (Right (TS.ParameterListAndReturnType _mTypeParameters _parameters mType1))) (TS.PropertySignature name2 _mOptional mType2) =
  name1 == name2 && compareMaybes compareTypes mType1 mType2
overridesFromInterface_ _ _ = False

--overloadsFromClass :: TS.AmbientClassBodyElement -> [TS.AmbientClassBodyElement] -> Bool
--overloadsFromClass function@TS.AmbientMemberDeclaration {} inherited = any (overloadsFromClass_ function) inherited
--overloadsFromClass _ _ = False
--
--overloadsFromClass_ :: TS.AmbientClassBodyElement -> TS.AmbientClassBodyElement -> Bool
--overloadsFromClass_ type1 inheritedType =
--  ambientClassBodyElementName type1 == ambientClassBodyElementName inheritedType
--  && compareMaybes compareForOverloadParameterListAndReturnType (ambientClassBodyElementParams type1) (ambientClassBodyElementParams inheritedType)
--
--overloadsFromInterface :: TS.AmbientClassBodyElement -> [TS.TypeMember] -> Bool
--overloadsFromInterface function@TS.AmbientMemberDeclaration {} inherited = any (overloadsFromInterface_ function) inherited
--overloadsFromInterface _ _ = False
--
--overloadsFromInterface_ :: TS.AmbientClassBodyElement -> TS.TypeMember -> Bool
--overloadsFromInterface_ (TS.AmbientMemberDeclaration _mPublicOrPrivate _mStatic name1 (Right parameterListAndReturnType1)) (TS.MethodSignature name2 _mOptional parameterListAndReturnType2) =
--  name1 == name2
--  {-- Params are equal, return types differ--}
--  && compareForOverloadParameterListAndReturnType (Right parameterListAndReturnType1) (Right parameterListAndReturnType2)
--overloadsFromInterface_ (TS.AmbientMemberDeclaration _mPublicOrPrivate _mStatic name1 (Left mType1)) (TS.PropertySignature name2 _mOptional mType2) =
--  name1 == name2
--  {-- Types differ! --}
--  && not (compareMaybes compareTypes mType1 mType2)
--overloadsFromInterface_ (TS.AmbientMemberDeclaration _mPublicOrPrivate _mStatic name1 (Right (TS.ParameterListAndReturnType _mTypeParameters _parameters mType1))) (TS.PropertySignature name2 _mOptional mType2) =
--  name1 == name2
--  {-- Types differ! --}
--  && not (compareMaybes compareTypes mType1 mType2)
--overloadsFromInterface_ _ _ = False
ambientClassBodyElementName :: TS.AmbientClassBodyElement -> String
ambientClassBodyElementName (TS.AmbientConstructorDeclaration _) = "<constructor>"
ambientClassBodyElementName (TS.AmbientMemberDeclaration _mPublicOrPrivate _mStatic name _eitherMTypeOrParameterListAndReturnType) =
  name
ambientClassBodyElementName (TS.AmbientIndexSignature (TS.IndexSignature name _stringOrNumber _type)) = name

ambientInterfaceBodyElementName :: TS.TypeMember -> String
ambientInterfaceBodyElementName (TS.PropertySignature name _ _) = name
ambientInterfaceBodyElementName (TS.CallSignature _) = "<call>"
ambientInterfaceBodyElementName TS.ConstructSignature {} = "<construct>"
ambientInterfaceBodyElementName (TS.TypeIndexSignature (TS.IndexSignature name _ _)) = name
ambientInterfaceBodyElementName (TS.MethodSignature name _ _) = name

ambientClassBodyElementParams ::
     TS.AmbientClassBodyElement -> Maybe (Either (Maybe TS.Type) TS.ParameterListAndReturnType)
ambientClassBodyElementParams (TS.AmbientConstructorDeclaration _) = Nothing
ambientClassBodyElementParams (TS.AmbientMemberDeclaration _mPublicOrPrivate _mStatic _name eitherMTypeOrParameterListAndReturnType) =
  Just eitherMTypeOrParameterListAndReturnType
ambientClassBodyElementParams (TS.AmbientIndexSignature (TS.IndexSignature _name _stringOrNumber _type)) = Nothing

--instance Eq DeclarationElement where
--  InterfaceDeclaration _ mExported1 interface1 == InterfaceDeclaration _ mExported2 interface2 =
--    mExported1 == mExported2 && interface1 == interface2
--  TypeAliasDeclaration _ mExported1 typeAlias1 == TypeAliasDeclaration _ mExported2 typeAlias2 =
--    mExported1 == mExported2 && typeAlias1 == typeAlias2
--  ExportDeclaration name1 == ExportDeclaration name2 =
--    name1 == name2
--  AmbientDeclaration _ mExported1 ambient1 == AmbientDeclaration _ mExported2 ambient2 =
--    mExported1 == mExported2 && ambient1 == ambient2
--  _ == _ = False
--instance Eq Interface where
--  Interface _ name1 mTypeParameters1 mTypeRefs1 typeBody1 ==
--    Interface _ name2 mTypeParameters2 mTypeRefs2 typeBody2 =
--      name1 == name2
--      && mTypeParameters1 == mTypeParameters2
--      && mTypeRefs1 == mTypeRefs2
--      && typeBody1 == typeBody2
compareTypeNames :: TS.TypeName -> TS.TypeName -> Bool
compareTypeNames (TS.TypeName Nothing name1) (TS.TypeName Nothing name2) = name1 == name2
compareTypeNames (TS.TypeName (Just moduleName1) name1) (TS.TypeName (Just moduleName2) name2) =
  compareModuleNames moduleName1 moduleName2 && name1 == name2
compareTypeNames _ _ = False

compareModuleNames :: TS.ModuleName -> TS.ModuleName -> Bool
compareModuleNames (TS.ModuleName names1) (TS.ModuleName names2) = names1 == names2

compareParameterListAndReturnType ::
     Either (Maybe TS.Type) TS.ParameterListAndReturnType
  -> Either (Maybe TS.Type) TS.ParameterListAndReturnType
  -> Bool
compareParameterListAndReturnType (Right (TS.ParameterListAndReturnType mTypeParameters1 parameters1 mType1)) (Right (TS.ParameterListAndReturnType mTypeParameters2 parameters2 mType2)) =
  let result =
        compareMaybes compareTypeParameters mTypeParameters1 mTypeParameters2 &&
        compareLists compareParameterTuples parameters1 parameters2 && compareMaybes compareTypes mType1 mType2
   in result
--  trace (
--    "type params:: " ++ show mTypeParameters1 ++ "<->" ++ show mTypeParameters2
--    ++ "\n"
--    ++ "params:: " ++ show parameters1 ++ "<->" ++ show parameters2
--    ++ "\n"
--    ++ "type:: " ++ show mType1 ++ "<->" ++ show mType2
--    ++ "\n"
--    ++ show result
--  )
compareParameterListAndReturnType (Left mType1) (Left mType2) = compareMaybes compareTypes mType1 mType2
compareParameterListAndReturnType _ _ = False

--{--| Params are equal, return types differ
-- -}
--compareForOverloadParameterListAndReturnType :: Either (Maybe TS.Type) TS.ParameterListAndReturnType -> Either (Maybe TS.Type) TS.ParameterListAndReturnType -> Bool
--compareForOverloadParameterListAndReturnType (Right (TS.ParameterListAndReturnType mTypeParameters1 parameters1 mType1)) (Right (TS.ParameterListAndReturnType mTypeParameters2 parameters2 mType2)) =
--    compareMaybes compareTypeParameters mTypeParameters1 mTypeParameters2
--    && compareLists compareParameterTuples parameters1 parameters2
--    {-- Return types differ --}
--    && not (compareMaybes compareTypes mType1 mType2)
--compareForOverloadParameterListAndReturnType (Left mType1) (Left mType2) =
--  {-- Return types differ! --}
--  not (compareMaybes compareTypes mType1 mType2)
--compareForOverloadParameterListAndReturnType _ _ = False
compareTypeParameters :: [TS.TypeParameter] -> [TS.TypeParameter] -> Bool
compareTypeParameters = compareLists compareTypeParameterTuples

compareTypeParameterTuples :: (TS.TypeParameter, TS.TypeParameter) -> Bool
compareTypeParameterTuples (TS.PartialTypeParameter mType1, TS.PartialTypeParameter mType2) =
  compareMaybes compareTypes mType1 mType2
compareTypeParameterTuples (TS.TypeParameter _ mType1, TS.TypeParameter _ mType2) =
  compareMaybes compareTypes mType1 mType2
compareTypeParameterTuples _ = False

compareTypes :: TS.Type -> TS.Type -> Bool
compareTypes _ (TS.Predefined _ TS.AnyType) = True
compareTypes (TS.Predefined _ predefinedType1) (TS.Predefined _ predefinedType2) =
  comparePredefinedTypes predefinedType1 predefinedType2
compareTypes (TS.TypeReference typeRef1) (TS.TypeReference typeRef2) = compareTypeRefs typeRef1 typeRef2
compareTypes (TS.ObjectType typeBody1) (TS.ObjectType typeBody2) = compareTypeBodies typeBody1 typeBody2
compareTypes (TS.ArrayType type1) (TS.ArrayType type2) = compareTypes type1 type2
compareTypes (TS.UnionType type1 type2) (TS.UnionType type3 type4) =
  compareTypes type1 type3 && compareTypes type2 type4
compareTypes (TS.TupleType types1) (TS.TupleType types2) = compareLists compareTupleTypes types1 types2
compareTypes (TS.TypeQuery queries1) (TS.TypeQuery queries2) = queries1 == queries2
compareTypes (TS.FunctionType mTypeParameters1 parameters1 type1) (TS.FunctionType mTypeParameters2 parameters2 type2) =
  compareMaybes compareTypeParameters mTypeParameters1 mTypeParameters2 &&
  compareLists compareParameterTuples parameters1 parameters2 && compareTypes type1 type2
compareTypes (TS.ConstructorType mTypeParameters1 parameters1 type1) (TS.ConstructorType mTypeParameters2 parameters2 type2) =
  compareMaybes compareTypeParameters mTypeParameters1 mTypeParameters2 &&
  compareLists compareParameterTuples parameters1 parameters2 && compareTypes type1 type2
compareTypes _ _ = False

compareTupleTypes :: (TS.Type, TS.Type) -> Bool
compareTupleTypes (type1, type2) = compareTypes type1 type2

compareParameterTuples :: (TS.Parameter, TS.Parameter) -> Bool
compareParameterTuples (TS.RequiredOrOptionalParameter mPublicOrPrivate1 _name1 mOptional1 mParameterType1, TS.RequiredOrOptionalParameter mPublicOrPrivate2 _name2 mOptional2 mParameterType2) =
  compareMaybes comparePublicOrPrivate mPublicOrPrivate1 mPublicOrPrivate2 &&
  compareMaybes compareOptional mOptional1 mOptional2 &&
  compareMaybes compareParameterTypes mParameterType1 mParameterType2
compareParameterTuples (TS.RestParameter _name1 mType1, TS.RestParameter _name2 mType2) =
  compareMaybes compareTypes mType1 mType2
compareParameterTuples _ = False

comparePublicOrPrivate :: TS.PublicOrPrivate -> TS.PublicOrPrivate -> Bool
comparePublicOrPrivate TS.Public TS.Public   = True
comparePublicOrPrivate TS.Private TS.Private = True
comparePublicOrPrivate _ _                   = False

compareOptional :: TS.Optional -> TS.Optional -> Bool
compareOptional TS.Optional TS.Optional = True

compareParameterTypes :: TS.ParameterType -> TS.ParameterType -> Bool
compareParameterTypes (TS.ParameterType type1) (TS.ParameterType type2) = compareTypes type1 type2
compareParameterTypes (TS.ParameterSpecialized typeName1) (TS.ParameterSpecialized typeName2) = typeName1 == typeName2
compareParameterTypes _ _ = False

comparePredefinedTypes :: TS.PredefinedType -> TS.PredefinedType -> Bool
comparePredefinedTypes TS.AnyType TS.AnyType               = True
comparePredefinedTypes (TS.NumberType _) (TS.NumberType _) = True
comparePredefinedTypes TS.BooleanType TS.BooleanType       = True
comparePredefinedTypes (TS.StringType _) (TS.StringType _) = True
comparePredefinedTypes TS.NullType TS.NullType             = True
comparePredefinedTypes TS.VoidType TS.VoidType             = True
comparePredefinedTypes _ _                                 = False

compareTypeRefs :: TS.TypeRef -> TS.TypeRef -> Bool
compareTypeRefs (TS.TypeRef typeName1 (Just types1)) (TS.TypeRef typeName2 (Just types2)) =
  compareTypeNames typeName1 typeName2 && compareLists compareTypeTuples types1 types2
compareTypeRefs (TS.TypeRef typeName1 Nothing) (TS.TypeRef typeName2 Nothing) = compareTypeNames typeName1 typeName2
compareTypeRefs _ _ = False

compareTypeTuples :: (TS.Type, TS.Type) -> Bool
compareTypeTuples (type1, type2) = compareTypes type1 type2

compareTypeBodies :: TS.TypeBody -> TS.TypeBody -> Bool
compareTypeBodies (TS.TypeBody typeMembers1) (TS.TypeBody typeMembers2) =
  compareLists compareTypeMembers typeMembers1 typeMembers2

compareTypeMembers :: ((TS.CommentPlaceholder, TS.TypeMember), (TS.CommentPlaceholder, TS.TypeMember)) -> Bool
compareTypeMembers ((_, typeMember1), (_, typeMember2)) = compareTypeMembers_ typeMember1 typeMember2

compareTypeMembers_ :: TS.TypeMember -> TS.TypeMember -> Bool
compareTypeMembers_ (TS.PropertySignature _name1 mOptional1 mType1) (TS.PropertySignature _name2 mOptional2 mType2) =
  compareMaybes compareOptional mOptional1 mOptional2 && compareMaybes compareTypes mType1 mType2
compareTypeMembers_ (TS.CallSignature parameterListAndReturnType1) (TS.CallSignature parameterListAndReturnType2) =
  compareParameterListAndReturnType (Right parameterListAndReturnType1) (Right parameterListAndReturnType2)
compareTypeMembers_ (TS.ConstructSignature (Just typeParameters1) parameters1 mType1) (TS.ConstructSignature (Just typeParameters2) parameters2 mType2) =
  compareLists compareTypeParameterTuples typeParameters1 typeParameters2 &&
  compareLists compareParameterTuples parameters1 parameters2 && compareMaybes compareTypes mType1 mType2
compareTypeMembers_ (TS.ConstructSignature Nothing parameters1 mType1) (TS.ConstructSignature Nothing parameters2 mType2) =
  compareLists compareParameterTuples parameters1 parameters2 && compareMaybes compareTypes mType1 mType2
compareTypeMembers_ (TS.TypeIndexSignature indexSignature1) (TS.TypeIndexSignature indexSignature2) =
  compareIndexSignatures indexSignature1 indexSignature2
compareTypeMembers_ (TS.MethodSignature _name1 (Just TS.Optional) parameterListAndReturnType1) (TS.MethodSignature _name2 (Just TS.Optional) parameterListAndReturnType2) =
  compareParameterListAndReturnType (Right parameterListAndReturnType1) (Right parameterListAndReturnType2)
compareTypeMembers_ (TS.MethodSignature _name1 Nothing parameterListAndReturnType1) (TS.MethodSignature _name2 Nothing parameterListAndReturnType2) =
  compareParameterListAndReturnType (Right parameterListAndReturnType1) (Right parameterListAndReturnType2)
compareTypeMembers_ _ _ = False

compareIndexSignatures :: TS.IndexSignature -> TS.IndexSignature -> Bool
compareIndexSignatures (TS.IndexSignature _name1 stringOrNumber1 type1) (TS.IndexSignature _name2 stringOrNumber2 type2) =
  compareStringOrNumbers stringOrNumber1 stringOrNumber2 && compareTypes type1 type2

compareStringOrNumbers :: TS.StringOrNumber -> TS.StringOrNumber -> Bool
compareStringOrNumbers TS.String TS.String = True
compareStringOrNumbers TS.Number TS.Number = True
compareStringOrNumbers _ _                 = False

compareMaybes :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
compareMaybes _ Nothing Nothing     = True
compareMaybes f (Just v1) (Just v2) = f v1 v2
compareMaybes _ _ _                 = False

compareLists :: ((a, a) -> Bool) -> [a] -> [a] -> Bool
compareLists _ [] [] = True
compareLists f l1@(_:_) l2@(_:_)
  | length l1 == length l2 =
    let tuples = zip l1 l2
     in all f tuples
  | otherwise = False
compareLists _ _ _ = False

returnTypeName :: Either (Maybe TS.Type) TS.ParameterListAndReturnType -> Maybe String
returnTypeName (Left (Just type_)) = Just $ returnTypeName_ type_
returnTypeName (Right (TS.ParameterListAndReturnType _ _ (Just type_))) = Just $ returnTypeName_ type_
returnTypeName _ = Nothing

returnTypeName_ :: TS.Type -> String
returnTypeName_ (TS.Predefined _ predefinedType) = predefinedReturnTypeName predefinedType
returnTypeName_ (TS.TypeReference (TS.TypeRef (TS.TypeName _ typeRefName) _)) = typeRefName
returnTypeName_ (TS.ObjectType _) = "Object"
returnTypeName_ (TS.ArrayType _) = "Array"
returnTypeName_ (TS.UnionType _ _) = "Union"
returnTypeName_ (TS.TupleType _) = "Tuple"
returnTypeName_ (TS.TypeQuery _) = "TypeQuery"
returnTypeName_ TS.FunctionType {} = "Function"
returnTypeName_ TS.ConstructorType {} = "Constructor"

predefinedReturnTypeName :: TS.PredefinedType -> String
predefinedReturnTypeName TS.AnyType        = "Any"
predefinedReturnTypeName (TS.NumberType _) = "Number"
predefinedReturnTypeName TS.BooleanType    = "Boolean"
predefinedReturnTypeName (TS.StringType _) = "String"
predefinedReturnTypeName TS.NullType       = "Null"
predefinedReturnTypeName TS.VoidType       = "Void"

reduceTripleToTuple :: ([a], [a], [b]) -> ([a], [b])
reduceTripleToTuple (xs, ys, rest) = (xs ++ ys, rest)

--
data AmbientContext = AmbientContext
  { typeAliases           :: [(String, TS.Type)]
  , declarations          :: [TS.DeclarationElement]
  , inheritedDeclarations :: [TS.DeclarationElement]
  } deriving (Show)

instance Semigroup AmbientContext where
  ctx1 <> ctx2 = ctx1 `mappend` ctx2

instance Monoid AmbientContext where
  mempty = AmbientContext [] [] []
  AmbientContext ta1 decl1 inheritedDecl1 `mappend` AmbientContext ta2 decl2 inheritedDecl2 =
    AmbientContext
      { typeAliases = ta1 ++ ta2 -- TODO: nub doesn't work here
      , declarations = decl1 ++ decl2 -- TODO: nub doesn't work here
      , inheritedDeclarations = inheritedDecl1 ++ inheritedDecl2 -- TODO: nub doesn't work here
      }

--instance Monoid AmbientContext => Foldable AmbientContext where
--  foldr f z (AmbientContext aliases) = AmbientContext ( foldr f z aliases )
--  foldMap
--
emptyAmbientContext :: AmbientContext
emptyAmbientContext = AmbientContext {typeAliases = [], declarations = [], inheritedDeclarations = []}

mkAmbientContext :: [TS.DeclarationElement] -> AmbientContext
mkAmbientContext els =
  let initialContext = emptyAmbientContext
      allAliases = concatMap mkAliases els -- TODO: nub doesn't work
   in initialContext {typeAliases = allAliases, declarations = els}

mkAliases :: TS.DeclarationElement -> [(String, TS.Type)]
mkAliases (TS.AmbientDeclaration _ _ ambient) = mkAlias ambient
--mkAliases (TS.TypeAliasDeclaration comment1 mExported (TS.TypeAlias comment alias d@(TS.DynamicAliasType mTypes))) =
----  trace ("mkAliases: " ++ show mTypes) $
--  case mTypes of
--    Nothing -> [(alias, d)]
--    Just types ->
--      let firstType = head types
--      in
--        if all (firstType ==) $ tail types
--        then mkAliases (TS.TypeAliasDeclaration comment1 mExported (TS.TypeAlias comment alias firstType))
--        else [(alias, d)]
mkAliases _                                   = []

mkAlias :: TS.Ambient -> [(String, TS.Type)]
--mkAlias (TS.AmbientTypeAliasDeclaration (TS.TypeAlias comment alias d@(TS.DynamicAliasType mTypes))) =
----  trace ("mkAliases: " ++ show mTypes) $
--  case mTypes of
--    Nothing -> [(alias, d)]
--    Just types ->
--      let firstType = head types
--      in
--        if all (firstType ==) $ tail types
--        then mkAlias (TS.AmbientTypeAliasDeclaration (TS.TypeAlias comment alias firstType))
--        else [(alias, d)]
mkAlias (TS.AmbientTypeAliasDeclaration (TS.TypeAlias _ alias d)) = [(alias, d)]
mkAlias (TS.AmbientNamespaceDeclaration _ _ ambients) = foldr (\ambient acc -> mkAlias ambient ++ acc) [] ambients
mkAlias (TS.AmbientClassDeclaration _comment _name _ps _exts _impls els) = mkAlias' . snd =<< els
mkAlias _ = []

mkAlias' :: TS.AmbientClassBodyElement -> [(String, TS.Type)]
{-- Consider uppercase `functions` inside a class as a type alias --}
--mkAlias' (AmbientMemberDeclaration _isOverride _pubPriv {--static:--}Nothing name _overloadedName (Right (ParameterListAndReturnType mTypeParams params (Just type_))))
mkAlias' (TS.AmbientMemberDeclaration _pubPriv Nothing name (Right (TS.ParameterListAndReturnType mTypeParams params (Just type_)))) {--static:--}
  | not (null name) && isUpper (head name) = [(name, TS.FunctionType mTypeParams params type_)]
  | otherwise = []
--      trace ("mkAlias' for member '" ++ name ++ "'")
mkAlias' _ = []

mapAlias :: AmbientContext -> String -> Maybe TS.Type
mapAlias ambientContext = mapAlias_ ambientContext Nothing

mapAlias_ :: AmbientContext -> Maybe TS.Type -> String -> Maybe TS.Type
mapAlias_ ambientContext mPreviousAlias aliasedName =
  let aliases = typeAliases ambientContext
      maybeANewAlias = listToMaybe $ map snd $ filter (\(name, _t) -> name == aliasedName) aliases
--  trace ("mapAlias for " ++ aliasedName) $
   in case maybeANewAlias of
        Just newTypeRefAlias@(TS.TypeReference (TS.TypeRef (TS.TypeName _ newAliasName) _)) ->
          mapAlias_ ambientContext (Just newTypeRefAlias) newAliasName
        Just newAlias -> Just newAlias
        Nothing -> mPreviousAlias

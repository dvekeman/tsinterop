#!/usr/bin/env stack
-- stack --resolver lts-13.15 script

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Turtle
import Prelude hiding (FilePath)

import           Lib

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
  generate (T.unpack $ Turtle.format Turtle.fp src) (T.unpack $ Turtle.format Turtle.fp dest) "" False

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

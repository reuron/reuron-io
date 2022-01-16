{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Safe (Exception)
import Data.Text (Text)
import Grace.Interpret (Input(..), InterpretError)
import Grace.Location (Location(..))
import Grace.Pretty (Pretty(..))
import Grace.Type (Type(..))
import System.FilePath ((</>))
import Test.Tasty (TestTree)

import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.Except as Except
import qualified Data.Text as Text
import qualified Grace.Interpret as Interpret
import qualified Grace.Monotype as Monotype
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified Grace.Width as Width
import qualified Prettyprinter as Pretty
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Test.Tasty.Silver as Silver

pretty_ :: Pretty a => a -> Text
pretty_ x =
    Grace.Pretty.renderStrict False Width.defaultWidth
        (pretty x <> Pretty.hardline)

interpret
    :: Input -> IO (Either InterpretError (Type Location, Value.Value))
interpret input = Except.runExceptT (Interpret.interpret input)

throws :: Exception e => IO (Either e a) -> IO a
throws io = do
    result <- io

    case result of
        Left  e -> Exception.throw e
        Right a -> return a

fileToTestTree :: FilePath -> IO TestTree
fileToTestTree prefix = do
    let input              = prefix <> "-input.ffg"
    let expectedTypeFile   = prefix <> "-type.ffg"
    let expectedOutputFile = prefix <> "-output.ffg"
    let expectedStderrFile = prefix <> "-stderr.txt"

    let name = FilePath.takeBaseName input

    eitherResult <- interpret (Path input)

    case eitherResult of
        Left e -> do
            return
                (Tasty.testGroup name
                    [ Silver.goldenVsAction
                        (name <> " - error")
                        expectedStderrFile
                        (return (Text.pack (Exception.displayException e)))
                        id
                    ]
                )
        Right (inferred, value) -> do
            let generateTypeFile = return (pretty_ inferred)

            let generateOutputFile = return (pretty_ (Normalize.quote [] value))

            return
                (Tasty.testGroup name
                    [ Silver.goldenVsAction
                        (name <> " - type")
                        expectedTypeFile
                        generateTypeFile
                        id
                    , Silver.goldenVsAction
                        (name <> " - output")
                        expectedOutputFile
                        generateOutputFile
                        id
                    ]
                )

inputFileToPrefix :: FilePath -> Maybe FilePath
inputFileToPrefix inputFile =
    fmap Text.unpack (Text.stripSuffix "-input.ffg" (Text.pack inputFile))

directoryToTestTree :: FilePath -> IO TestTree
directoryToTestTree directory = do
    let name = FilePath.takeBaseName directory

    children <- Directory.listDirectory directory

    let process child = do
            let childPath = directory </> child

            isDirectory <- Directory.doesDirectoryExist childPath

            if isDirectory
                then do
                    testTree <- directoryToTestTree childPath

                    return [ testTree ]

                else do
                    case inputFileToPrefix childPath of
                        Just prefix -> do
                            testTree <- fileToTestTree prefix

                            return [ testTree ]

                        Nothing -> do
                            return [ ]

    testTreess <- traverse process children

    return (Tasty.testGroup name (concat testTreess))

main :: IO ()
main = do
    autogeneratedTestTree <- directoryToTestTree "tasty/data"

    let manualTestTree =
            Tasty.testGroup "Manual tests"
                [ interpretCode
                , interpretCodeWithEnvURI
                , interpretCodeWithFileURI
                , interpretCodeWithImport
                ]

    let tests = Tasty.testGroup "Tests" [ autogeneratedTestTree, manualTestTree ]

    Tasty.defaultMain tests

interpretCode :: TestTree
interpretCode = Tasty.HUnit.testCase "interpret code" do
    actualValue <- throws (interpret (Code "(input)" "2 + 2"))

    let expectedValue =
            (Type{ location, node }, Value.Scalar (Syntax.Natural 4))
          where
            location = Location{ name = "(input)", code = "2 + 2", offset = 0 }

            node = Type.Scalar Monotype.Natural

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithImport :: TestTree
interpretCodeWithImport = Tasty.HUnit.testCase "interpret code with import from file" do
    actualValue <- throws (interpret (Code "(input)" "./tasty/data/unit/plus-input.ffg"))

    let expectedValue =
            (Type{ location, node }, Value.Scalar (Syntax.Natural 5))
          where
            location = Location{ name = "tasty/data/unit/plus-input.ffg", code = "2 + 3\n", offset = 0 }

            node = Type.Scalar Monotype.Natural

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithEnvURI :: TestTree
interpretCodeWithEnvURI = Tasty.HUnit.testCase "interpret code with env: import" do
    let key = "GRACE_TEST_VAR"

    let name = "env:" <> key

    let open = do
            m <- Environment.lookupEnv key

            Environment.setEnv key "true"

            return m

    let close  Nothing  = Environment.unsetEnv key
        close (Just v ) = Environment.setEnv key v

    actualValue <- Exception.bracket open close \_ -> do
        throws (interpret (Code "(input)" (Text.pack name)))

    let expectedValue =
            (Type{ location, node }, Value.Scalar (Syntax.Bool True))
          where
            location = Location{ name, code = "true", offset = 0 }

            node = Type.Scalar Monotype.Bool

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithFileURI :: TestTree
interpretCodeWithFileURI = Tasty.HUnit.testCase "interpret code with file:// import" do
    absolute <- Directory.makeAbsolute "./tasty/data/true.ffg"

    let uri = "file://" <> absolute

    actualValue <- throws (interpret (Code "(input)" (Text.pack uri)))

    let expectedValue =
            (Type{ location, node }, Value.Scalar (Syntax.Bool True))
          where
            location = Location{ name = absolute, code = "true\n", offset = 0 }

            node = Type.Scalar Monotype.Bool

    Tasty.HUnit.assertEqual "" expectedValue actualValue

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-| This module contains the top-level `main` function that implements the
    command-line API
-}
module Grace
    ( -- * Main
      main
    ) where

import Control.Applicative (many, (<|>))
import Control.Exception.Safe (Exception(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Void (Void)
import Grace.Interpret (Input(..))
import Grace.Location (Location(..))
import Grace.Syntax (Builtin(..), Syntax(..))
import Grace.Type (Type(..))
import qualified Grace.Simulator as Simulator
import Options.Applicative (Parser, ParserInfo)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Control.Monad.Except as Except
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Grace.HTTP as HTTP
import qualified Grace.Infer as Infer
import qualified Grace.Interpret as Interpret
import qualified Grace.Monotype as Monotype
import qualified Grace.Normalize as Normalize
import Grace.Synonym (resugarType)
import qualified Grace.Parser as Parser
import qualified Grace.Pretty
import qualified Grace.REPL as REPL
import qualified Grace.Server as Server
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified Grace.Width as Width
import qualified Options.Applicative as Options
import qualified Prettyprinter as Pretty
import qualified System.Console.ANSI as ANSI
import qualified System.Exit as Exit
import qualified System.IO as IO

data Highlight
    = Color
    -- ^ Force the use of ANSI color escape sequences to highlight source code
    | Plain
    -- ^ Don't highlight source code
    | Auto
    -- ^ Auto-detect whether to highlight source code based on whether or not
    --   @stdout@ is a terminal

data Options
    = Interpret
      { annotate :: Bool
      , highlight :: Highlight
      , file :: FilePath
      }
    | ToSimulator { file :: FilePath }
    | Text { file :: FilePath }
    | Format { highlight :: Highlight, files :: [FilePath] }
    | Builtins { highlight :: Highlight }
    | REPL {}
    | Serve { port :: Int }

parserInfo :: ParserInfo Options
parserInfo =
    Options.info (Options.helper <*> parser)
        (Options.progDesc "Command-line utility for the Grace language")

parser :: Parser Options
parser = do
    let interpret = do
            annotate <- Options.switch
                (   Options.long "annotate"
                <>  Options.help "Add a type annotation for the inferred type"
                )

            file <- Options.strArgument
                (   Options.help "File to interpret"
                <>  Options.metavar "FILE"
                )

            highlight <- parseHighlight

            return Interpret{..}

    let toSimulator = do
            file <- Options.strArgument
                (   Options.help "File to interpret"
                <>  Options.metavar "FILE"
                )
            return ToSimulator{..}

    let text = do
            file <- Options.strArgument
                (   Options.help "File to interpret"
                <>  Options.metavar "FILE"
                )

            return Text{..}

    let format = do
            let parseFile =
                    Options.strArgument
                        (   Options.help "File to format"
                        <>  Options.metavar "FILE"
                        )

            highlight <- parseHighlight

            files <- many parseFile

            return Format{..}

    let builtins = do
            highlight <- parseHighlight

            return Builtins{..}

    let repl = do
            pure REPL{}

    let serve = do
          port <- Options.option Options.auto (Options.help "Port to serve on" <> Options.long "port")
          return $ Serve {..}

    Options.hsubparser
        (   Options.command "interpret"
                (Options.info interpret
                    (Options.progDesc "Interpret a Grace file")
                )


        <>  Options.command "to-simulator"
                (Options.info toSimulator
                    (Options.progDesc "Render a Grace value to a simulator json value")
                )

        <>  Options.command "text"
                (Options.info text
                    (Options.progDesc "Render a Grace text literal")
                )

        <>  Options.command "format"
                (Options.info format
                    (Options.progDesc "Format Grace code")
                )

        <>  Options.command "builtins"
                (Options.info builtins
                    (Options.progDesc "List all built-in functions and their types")
                )
        <> Options.command "repl"
                (Options.info repl
                    (Options.progDesc "Enter a REPL for Grace")
                )
        <> Options.command "serve"
                (Options.info serve
                    (Options.progDesc "Run a normalization server")
                )
        )
  where
    parseHighlight =
            Options.flag' Color
                (    Options.long "color"
                <>   Options.help "Enable syntax highlighting"
                )
        <|> Options.flag' Plain
                (    Options.long "plain"
                <>   Options.help "Disable syntax highlighting"
                )
        <|> pure Auto


detectColor :: Highlight -> IO Bool
detectColor Color = do return True
detectColor Plain = do return False
detectColor Auto  = do ANSI.hSupportsANSI IO.stdout

getRender :: Highlight -> IO (Doc AnsiStyle -> IO ())
getRender highlight = do
    color <- detectColor highlight
    width <- Width.getWidth

    return (Grace.Pretty.renderIO color width IO.stdout)

throws :: Exception e => Either e a -> IO a
throws (Left e) = do
    Text.IO.hPutStrLn IO.stderr (Text.pack (displayException e))
    Exit.exitFailure
throws (Right result) = do
    return result

-- | Command-line entrypoint
main :: IO ()
main = do
    options <- Options.execParser parserInfo

    case options of
        Interpret{..} -> do
            input <- case file of
                "-" -> do
                    Code "(input)" <$> Text.IO.getContents
                _ -> do
                    return (Path file)

            eitherResult <- do
                Except.runExceptT (Interpret.interpret input)

            (inferred, value) <- throws eitherResult

            let syntax = Normalize.quote [] value

            let annotatedExpression
                    | annotate =
                        Annotation
                            { annotated = syntax
                            , annotation = void inferred
                            , location = ()
                            }
                    | otherwise =
                        syntax

            render <- getRender highlight
            if annotate
              then
                BS.putStrLn $ Aeson.encode $ Aeson.object
                    [ "value" Aeson..= show (Grace.Pretty.pretty syntax)
                    , "type" Aeson..= show (Grace.Pretty.pretty (resugarType inferred))
                    ]
              else

                render (Grace.Pretty.pretty annotatedExpression <> Pretty.hardline)

        ToSimulator{..} -> do
            input <- case file of
                "-" -> do
                    Code "(input)" <$> Text.IO.getContents
                _ -> do
                    return (Path file)

            eitherResult <- do
                Except.runExceptT (Interpret.interpret input)

            (inferred, value) <- throws eitherResult

            let syntax = Normalize.quote [] $ Simulator.toSimulator value

            BS.putStrLn $ Aeson.encode $ Aeson.object
                [ "value" Aeson..= show (Grace.Pretty.pretty (syntax))
                , "type" Aeson..= show (Grace.Pretty.pretty (resugarType inferred))
                ]

        Text{..} -> do
            input <- case file of
                "-" -> do
                    Code "(input)" <$> Text.IO.getContents
                _ -> do
                    return (Path file)

            let location =
                    Location
                        { name = "(input)"
                        , code = "… : Text"
                        , offset = 4
                        }

            let expected = Type.Scalar{ scalar = Monotype.Text, .. }

            manager <- HTTP.newManager

            eitherResult <- do
                Except.runExceptT (Interpret.interpretWith [] (Just expected) manager input)

            (_, value) <- throws eitherResult

            case value of
                Value.Scalar (Syntax.Text text) -> Text.IO.putStr text
                _ -> do
                    Text.IO.hPutStrLn IO.stderr
                        "Internal error: Not a Text literal\n\
                        \\n\
                        \The input expression did not evaluate to a Text literal, even though it had the\n\
                        \correct type"
                    Exit.exitFailure

        Format{..} -> do
            case files of
                [ "-" ] -> do
                    text <- Text.IO.getContents

                    syntax <- throws (Parser.parse "(input)" text)

                    render <- getRender highlight

                    render (Grace.Pretty.pretty syntax <> Pretty.hardline)
                _ -> do
                    let formatFile file = do
                            text <- Text.IO.readFile file

                            syntax <- throws (Parser.parse file text)

                            IO.withFile file IO.WriteMode \handle -> do
                                Grace.Pretty.renderIO
                                    False
                                    Width.defaultWidth
                                    handle
                                    (Grace.Pretty.pretty syntax <> Pretty.hardline)

                    traverse_ formatFile files

        Builtins{..} -> do
            let displayBuiltin :: Builtin -> IO ()
                displayBuiltin builtin = do
                    let expression =
                            Syntax.Builtin
                                { location =
                                    Location
                                        { name = "(input)"
                                        , code =
                                            Grace.Pretty.renderStrict
                                                False
                                                Width.defaultWidth
                                                (Grace.Pretty.pretty builtin)
                                        , offset = 0
                                        }
                                , ..
                                }

                    type_ <- throws (Infer.typeOf expression)

                    let annotated :: Syntax Location Void
                        annotated =
                            Annotation
                                { annotated = expression
                                , annotation = type_
                                , location = Syntax.location expression
                                }

                    render <- getRender highlight

                    render (Grace.Pretty.pretty annotated <> Pretty.hardline)

            let builtins = [ minBound .. maxBound ]

            case builtins of
                [] -> do
                    return ()

                b0 : bs -> do
                    displayBuiltin b0

                    traverse_ (\b -> Text.IO.putStrLn "" >> displayBuiltin b) bs

        REPL{} -> do
            REPL.repl

        Serve{ .. } -> do
            Server.serve port

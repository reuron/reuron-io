-- |
{-# LANGUAGE OverloadedStrings #-}

module Grace.Server where

import qualified Control.Monad.Except as Except
import qualified Data.Text.Encoding as Text
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as HTTP

import qualified Data.ByteString.Lazy as BSL
import qualified Grace.Interpret as Interpret
import Grace.Input (Input(Code))
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty

-- TODO: Harden (maxinum body length, encoding)
serve :: Int -> IO ()
serve port = Warp.run port $ Cors.simpleCors $ \req respond -> do
  input <- Code "request" . Text.decodeUtf8 . BSL.toStrict <$> Wai.strictRequestBody req
  eitherResult <- Except.runExceptT (Interpret.interpret input)
  case eitherResult of
    Left _ -> error "TODO"
    Right (_ty, value) -> do
      let syntax = Normalize.quote [] value
      let response = Pretty.toText syntax
      respond $ Wai.responseLBS HTTP.status200 [] (BSL.fromStrict $ Text.encodeUtf8 response)

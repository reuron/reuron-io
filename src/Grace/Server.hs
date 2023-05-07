-- |
{-# LANGUAGE OverloadedStrings #-}

module Grace.Server where

import qualified Control.Monad.Except as Except
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.Wai.Application.Static as Wai

import qualified Data.ByteString.Builder as Builder
import qualified Grace.Interpret as Interpret
import Grace.Input (Input(Code))
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty

-- TODO: Harden (maxinum body length, encoding)
serve :: Int -> IO ()
serve port = Warp.run port $ Cors.simpleCors $ \req respond ->
  case Wai.pathInfo req of
    ["interpret"] -> do
      input <- Code "request" . Text.decodeUtf8 . LBS.toStrict <$> Wai.strictRequestBody req
      eitherResult <- Except.runExceptT (Interpret.interpret input)
      case eitherResult of
        Left e -> respond $ Wai.responseLBS HTTP.status400 [] (LBS.pack (show e))
        Right (_ty, value) -> do
          let syntax = Normalize.quote [] value
          let response = Pretty.toText syntax
          respond $ Wai.responseLBS HTTP.status200 [] (LBS.fromStrict $ Text.encodeUtf8 response)
    ["convert-swc"] -> do
      swcBytes <- Wai.lazyRequestBody req
      swcContents <- case HTTP.parseRequest (LBS.unpack swcBytes) of
            Nothing -> return swcBytes
            Just url -> do
              mgr <- HTTP.newTlsManager
              resp <- HTTP.httpLbs url mgr
              let body = HTTP.responseBody resp
              return $ body
      let ffgBytes = swcContentsToFfg swcContents
      respond $ Wai.responseBuilder HTTP.status200 [] ffgBytes
    ["echo"] -> do
      bytes <- Wai.lazyRequestBody req
      respond $ Wai.responseLBS HTTP.status200 [] bytes
    _ -> Wai.staticApp (Wai.defaultWebAppSettings "static") req respond
    -- _ -> respond $ Wai.responseLBS HTTP.status404 [] "Not found"


swcContentsToFfg :: LBS.ByteString -> Builder.Builder
swcContentsToFfg input =
  header
  <> (mconcat (swcLine <$> LBS.lines input))
  <> footer
  where
    header :: Builder.Builder
    header =
      "let defaultSwcNeuron = \
      \  https://raw.githubusercontent.com/imalsogreg/reuron/main/data/defaultSwcNeuron.ffg \
      \in \n defaultSwcNeuron [ \
      \"

    swcLine :: LBS.ByteString -> Builder.Builder
    swcLine line =
      let trim str = LBS.filter (/= ' ') str
      in
      case LBS.words line of
        [nId, nType, x, y, z, radius, parent] ->
          Builder.byteString . BS.pack . concat $
          ["  {id: ",    LBS.unpack (trim nId)
          ,", x: ",      LBS.unpack (trim x)
          ,", y: ",      LBS.unpack (trim y)
          ,", z: ",      LBS.unpack (trim z)
          ,", r: ",      LBS.unpack (trim radius)
          ,", type: ",   LBS.unpack (trim nType)
          ,", parent: ", LBS.unpack (trim parent)
          ,"},\n"
          ]
        _ -> mempty



    footer :: Builder.Builder
    footer = "\n]"

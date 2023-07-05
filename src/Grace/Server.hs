-- |
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Grace.Server where

import qualified Control.Monad.Except as Except
import qualified Data.Text.Encoding as Text
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Scientific (Scientific)
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
      let simplifyDegree = case List.lookup "degree" (Wai.queryString req) of
            Just (Just bs) -> Just $ read @Int (BS.unpack bs)
            Just Nothing -> error "Invalid degree parameter"
            Nothing -> Nothing
      swcContents <- case HTTP.parseRequest (LBS.unpack swcBytes) of
            Nothing -> return swcBytes
            Just url -> do
              mgr <- HTTP.newTlsManager
              resp <- HTTP.httpLbs url mgr
              let body = HTTP.responseBody resp
              return $ body
      let
        swcLines = parseSwcLines swcContents
        simplifiedSwcLines = case simplifyDegree of
          Nothing -> swcLines
          Just n -> simplifySwc n swcLines
        ffgBytes = encodeSwcLines simplifiedSwcLines

      respond $ Wai.responseBuilder HTTP.status200 [] ffgBytes
    ["echo"] -> do
      bytes <- Wai.lazyRequestBody req
      respond $ Wai.responseLBS HTTP.status200 [] bytes
    [] -> do
      index <- LBS.readFile "static/index.html"
      respond $ Wai.responseLBS HTTP.status200 [] index
    _ -> Wai.staticApp (Wai.defaultWebAppSettings "static") req respond

parseSwcLines :: LBS.ByteString -> [SwcLine]
parseSwcLines swcBytes = mapMaybe parseLine (LBS.lines swcBytes)
  where
    parseLine :: LBS.ByteString -> Maybe SwcLine
    parseLine line =
      let trim str = LBS.filter (/= ' ') str
      in
      case LBS.words line of
        [nId', nType', x', y', z', radius', parent'] -> Just $ SwcLine
          { id_ = read (LBS.unpack $ trim nId')
          , x   = read (LBS.unpack $ trim x')
          , y   = read (LBS.unpack $ trim y')
          , z   = read (LBS.unpack $ trim z')
          , r   = read (LBS.unpack $ trim radius')
          , parent_ = read (LBS.unpack $ trim parent')
          , type_ = read (LBS.unpack $ trim nType')
          }
        _ -> Nothing -- error "bad line"


encodeSwcLines :: [SwcLine] -> Builder.Builder
encodeSwcLines swcLines =
  header
  <> (mconcat (swcLine <$> swcLines))
  <> footer
  where
    header :: Builder.Builder
    header =
      "let defaultSwcNeuron = \
      \  https://raw.githubusercontent.com/reuron/reuron-lib/main/defaultSwcNeuron.ffg \
      \in \n defaultSwcNeuron [ \
      \"

    swcLine :: SwcLine  -> Builder.Builder
    swcLine SwcLine { id_, x, y, z, r, type_, parent_ } =
        Builder.byteString . BS.pack . concat $
        ["  {id: ",    show id_
        ,", x: ",      show x
        ,", y: ",      show y
        ,", z: ",      show z
        ,", r: ",      show r
        ,", type: ",   show type_
        ,", parent: ", show parent_
        ,"},\n"
        ]


    footer :: Builder.Builder
    footer = "\n]"


data SwcLine = SwcLine
  { id_ :: Int
  , x :: Scientific
  , y :: Scientific
  , z :: Scientific
  , r :: Scientific
  , type_ :: Int
  , parent_ :: Int
  }

simplifySwc :: Int -> [SwcLine] -> [SwcLine]
simplifySwc degree swcLines =
  let
    children :: Map Int [Int]
    children =
      List.foldl'
      (\acc SwcLine{id_, parent_} -> Map.insertWith (++) parent_ [id_] acc)
      mempty
      swcLines

    entities :: Map Int SwcLine
    entities = Map.fromList [(id_, x) | x@SwcLine{id_} <- swcLines]

    keepableLines :: [SwcLine]
    keepableLines = List.filter keep swcLines
      where
        keep SwcLine {id_} =
          let
            is_first = id_ == 1
            is_branch_or_leaf =
              case Map.lookup id_ children of
                Nothing -> True
                Just n -> List.length n > 1
            is_downsample = id_ `mod` degree == 0
          in is_first
             || is_branch_or_leaf
             || is_downsample

    shouldKeep :: Set Int
    shouldKeep = Set.fromList $ map id_ $ keepableLines

    -- TODO: Log a warning when returning -1.
    seekKeepableEntity :: Int -> Int
    seekKeepableEntity eid = case Map.lookup eid entities of
      Just SwcLine { parent_ } ->
        if eid `Set.member` shouldKeep
        then eid
        else seekKeepableEntity parent_
      Nothing -> -1 --error $ "Unknown entity " ++ show eid

    adjustParent :: SwcLine -> SwcLine
    adjustParent s@SwcLine {parent_} =
      if parent_ == -1
      then s -- Entities with no parent don't get parent adjustment.
      else s { parent_ = seekKeepableEntity parent_ }

  in
    map adjustParent $ keepableLines

-- |
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}

module Grace.FFI where

import Control.Exception.Safe (displayException)
import qualified Control.Monad.Except as Except
import Data.Text (pack)
import Foreign (poke)
import Foreign.C (CString, CInt(..))
import Foreign.C.String (newCString, peekCString)
import Foreign.Ptr (Ptr)

import Grace.Pretty (pretty)
import qualified Grace.Normalize as Normalize
import Grace.Interpret (Input(Code), interpret)

-- | Expose a c ABI functino that interprets grace code in a CString,
--   writing the output, type, and error message into the three output parameters.
--   Returns 0 on success and 1 on failure.
c_interpret :: CString -> Ptr CString -> Ptr CString -> Ptr CString -> IO CInt
c_interpret input outputPtr typePtr errorPtr = do
  inputHs <- Code "(input)" . pack <$> peekCString input
  result <- Except.runExceptT (interpret inputHs)
  case result of
    Left e -> do
      newCString (displayException e) >>= poke errorPtr
      return 1
    Right (inferred, value) -> do
      newCString (show $ pretty $ (Normalize.quote [] value)) >>= poke outputPtr
      newCString (show $ pretty inferred) >>= poke typePtr
      return 0

foreign export ccall c_interpret :: CString -> Ptr CString -> Ptr CString -> Ptr CString -> IO CInt

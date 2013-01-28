module IO (
    WriteFormat (..)
  , readFile
  , writeFile
) where

import Prelude hiding ( readFile, writeFile )

import Types
import Raw

import Control.Applicative
import Control.Monad
import Control.Exception

import Foreign
import Foreign.C


data ReadChannels (c :: Chan)

instance Format (ReadChannels C1) where tag _ = c'CCV_IO_GRAY
instance Format (ReadChannels C3) where tag _ = c'CCV_IO_RGB_COLOR


readFile :: forall c. (Format c, Format (ReadChannels c))
         => FilePath -> IO (DenseMatrix C8U c)
readFile path =
  withCString path $ \ppath ->
  with nullPtr     $ \ppmat -> do
    c'ccv_read_impl (castPtr ppath) ppmat flags 0 0 0 >>= throw_non_final
    peek ppmat >>= newMatrixCCVAlloc

  where
    flags = c'CCV_IO_ANY_FILE .|. tag (Proxy :: Proxy (C8U * ReadChannels c))

-- XXX:
-- ccv_read: IO_ANY_STREAM, IO_ANY_RAW

data WriteFormat = JPEG | PNG | Binary
  deriving (Eq, Show)

writeFile :: FilePath -> WriteFormat -> DenseMatrix e c -> IO ()
writeFile path fmt mat =
  withCString path   $ \ppath ->
  unsafeWithRepr mat $ \pmat  ->
    c'ccv_write pmat ppath nullPtr (decode fmt) nullPtr >>= throw_non_final
  where
    decode JPEG   = c'CCV_IO_JPEG_FILE
    decode PNG    = c'CCV_IO_PNG_FILE
    decode Binary = c'CCV_IO_BINARY_FILE

-- 

pass_non_final :: CInt -> IO (Maybe CCV_IO_Exn)
pass_non_final s
  | s == c'CCV_IO_FINAL = return Nothing
  | otherwise           = return $ (Just . CCV_IO_Exn . fromIntegral) s

throw_non_final :: CInt -> IO ()
throw_non_final = pass_non_final >=> return () `maybe` throwIO


module Memory (
  enableCache, disableCache, drainCache, unsafeDenseMatrixNew
) where

import Types
import FfiUtils
import Raw

import Foreign

enableCache :: Maybe Int -> IO ()
enableCache Nothing     = c'ccv_enable_default_cache
enableCache (Just size) = c'ccv_enable_cache (_fi size)

disableCache :: IO ()
disableCache = c'ccv_disable_cache

drainCache :: IO ()
drainCache = c'ccv_drain_cache

unsafeDenseMatrixNew :: forall e c . (Format (e * c))
               => Int -> Int
               -> ForeignPtr ()
               -> IO (DenseMatrix e c)
unsafeDenseMatrixNew rows cols fp =
  withForeignPtr fp $ \pdata -> do
    pmat <- c'ccv_dense_matrix_new (_fi rows) (_fi cols) flags pdata 0
    c'ccv_make_matrix_immutable pmat
    newMatrixOwnAlloc pmat fp

  where
    flags = c'CCV_NO_DATA_ALLOC .|. tag (Proxy :: Proxy (e * c))


module AuxTypes (
    CCVT (..)
  , withCCVArray'
  , outCCVArray'
  , withCCVArray
  , outCCVArray
) where

import Types
import Raw
import FfiUtils

import Control.Monad
import Control.Applicative
import Control.Arrow

import Control.Exception

import Foreign hiding ( unsafePerformIO )
import Foreign.Marshal.Array


-- Class for pure conversion between FFI-generated types and sane Haskell
-- equivalents.

class Storable t => CCVT t where
  type HsT t :: *
  hs2ccv :: HsT t -> t
  ccv2hs :: t -> HsT t

instance CCVT C'ccv_decimal_point_t where
  type HsT C'ccv_decimal_point_t = (Float, Float)
  hs2ccv (x, y) = C'ccv_decimal_point_t (_rf x) (_rf y)
  ccv2hs (C'ccv_decimal_point_t x y) = (_rf x, _rf y)

instance CCVT C'ccv_decimal_point_with_status_t where
  type HsT C'ccv_decimal_point_with_status_t = ((Float, Float), Bool)
  hs2ccv (pt, f) = C'ccv_decimal_point_with_status_t
                    (hs2ccv pt) (if f then 1 else 0)
  ccv2hs (C'ccv_decimal_point_with_status_t pt f) = (ccv2hs pt, f /= 0)

instance CCVT C'ccv_size_t where
  type HsT C'ccv_size_t = (Int, Int)
  hs2ccv (x, y) = C'ccv_size_t (_fi x) (_fi y)
  ccv2hs (C'ccv_size_t x y) = (_fi x, _fi y)


-- Nasty parameter-less CCV containers.

peekCCVArray :: forall a. Storable a => C'ccv_array_t -> IO [a]
peekCCVArray arr
  | size /= size' = error $ "peekCCVArray: element size:" ++ show size' ++
                            ", requested: "               ++ show size
  | otherwise     = peekArray (_fi     $ c'ccv_array_t'rnum arr)
                              (castPtr $ c'ccv_array_t'data arr)
  where
    (size, size') = ( sizeOf (undefined :: a)
                    , _fi (c'ccv_array_t'rsize arr) )

newCCVArray :: forall a. Storable a => [a] -> IO (Ptr C'ccv_array_t)
newCCVArray xs = do
  arr <- c'ccv_array_new (_fi $ sizeOf (undefined :: a))
                         (_fi $ length xs) 0
  arr <$ mapM_ (`with` c'ccv_array_push arr) xs

withCCVArray' :: Storable a => [a] -> (Ptr C'ccv_array_t -> IO b) -> IO b
withCCVArray' xs = bracket (newCCVArray xs) c'ccv_array_free_immediately

outCCVArray' :: Storable a => (Ptr (Ptr C'ccv_array_t) -> IO b) -> IO (b, [a])
outCCVArray' f = with nullPtr $ \pparr ->
  (,) <$> f pparr <*> do
    parr <- peek pparr
    (peek parr >>= peekCCVArray) <* c'ccv_array_free parr

withCCVArray :: forall a b t. (CCVT t, HsT t ~ a)
              => Proxy t -> [a] -> (Ptr C'ccv_array_t -> IO b) -> IO b
withCCVArray _ xs f = withCCVArray' (map (hs2ccv :: a -> t) xs) f

outCCVArray :: forall a b t. (CCVT t, HsT t ~ a)
             => Proxy t -> (Ptr (Ptr C'ccv_array_t) -> IO b) -> IO (b, [a])
outCCVArray _ f = second ((ccv2hs :: t -> a) <$>) <$> outCCVArray' f


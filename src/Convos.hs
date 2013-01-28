module Convos (
  toVector, fromVector, toMatrix, fromMatrix
) where

import Raw
import Types
import Memory

import Control.Applicative
import Data.List ( intersperse )

import qualified Data.Vector.Storable as V
import qualified Data.Packed.Matrix as M

import Foreign hiding ( unsafePerformIO )
import System.IO.Unsafe

--
import Debug.Trace


class (Format e, Storable (Native e), Num (Native e))
      => Element (e :: Cell) where
  type Native c :: *

instance Element C8U  where type Native C8U  = Word8
instance Element C32S where type Native C32S = Int32
instance Element C32F where type Native C32F = Float
instance Element C64S where type Native C64S = Int64
instance Element C64F where type Native C64F = Double

class (Format c) => NChan (c :: Chan) where
  nChan :: Proxy c -> Int

instance NChan C1 where nChan _ = 1
instance NChan C2 where nChan _ = 2
instance NChan C3 where nChan _ = 3
instance NChan C4 where nChan _ = 4


toRawVector :: forall e c. (Element e)
            => DenseMatrix e c
            -> IO (V.Vector (Native e))

toRawVector m = unsafeWithRepr m $ \p -> do

  -- XXX: how the assure the matrix is always immutable?
  sig <- peek $ p'ccv_dense_matrix_t'sig p
  assertIO (sig /= 0) "toRawVector: no signature - mutable matrix"

  c'_hccv_dense_matrix_ref p

  fp <- peek (p'ccv_dense_matrix_t'data p) >>=
          newForeignPtrEnv p'_hccv_dense_matrix_unref2 p . castPtr

  r <- fromIntegral <$> peek (p'ccv_dense_matrix_t'rows p)
  s <- fromIntegral <$> peek (p'ccv_dense_matrix_t'step p)

  return $! V.unsafeFromForeignPtr0 fp
         $  r * (s `quot` sizeOf (undefined :: Native e))

toVector :: forall e c. (Element e, NChan c)
         => DenseMatrix e c
         -> V.Vector (Native e)

toVector m = unsafePerformIO $ do
  v <- toRawVector m
  let v' | sizeOf (undefined :: Native e) > 1 = v
         | otherwise
         = V.concat [ V.unsafeSlice (i * step') cols'c v
                      | i <- [0 .. rows' - 1] ]
  return v'

  where
    (rows', step') = (rows m, step m)
    cols'c         = cols m * nChan (Proxy :: Proxy c)

fromVector :: forall e c. (Element e, NChan c)
           => Int -> Int -> V.Vector (Native e)
           -> DenseMatrix e c

fromVector rows cols v
  | V.length v < rows * cols'c = error "fromVector"
  | otherwise
  = unsafePerformIO $ unsafeDenseMatrixNew rows cols
                    $ castForeignPtr (fst $ V.unsafeToForeignPtr0 vec1)
  where
    vec1 | sizeOf (undefined :: Native e) > 1 = v
         | otherwise =
           V.concat $ intersperse row_padding
            [ V.unsafeSlice (i * cols'c) cols'c v
              | i <- [0 .. rows - 1] ]

    cols'c  = cols * nChan (Proxy :: Proxy c)

    row_padding = V.fromList $ replicate (padding cols'c) 0

padding :: Int -> Int
padding row_bytes = ( row_bytes + 3 ) .&. (-4) - row_bytes


toMatrix :: (Element e, NChan c, M.Element (Native e))
         => DenseMatrix e c -> M.Matrix (Native e)

toMatrix mat = M.reshape (cols mat) (toVector mat)

fromMatrix :: forall e c. (Element e, NChan c, M.Element (Native e))
           => M.Matrix (Native e) -> DenseMatrix e c

fromMatrix mat = fromVector (M.rows mat) cols (M.flatten mat)
  where (w, r) = M.cols mat `quotRem` nChan (Proxy :: Proxy c)
        cols | r == 0    = w
             | otherwise = error "fromMatrix"


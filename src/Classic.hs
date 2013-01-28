module Classic (
  Cell8U32S
  , canny
  , otsu
  , opticalFlow
) where

import Types
import FfiUtils
import AuxTypes
import Raw

import Control.Monad
import Control.Arrow

import Foreign hiding ( unsafePerformIO )
import System.IO.Unsafe

--  hog :: 

class Format c => Cell8U32S (c :: Cell)
instance Cell8U32S C8U
instance Cell8U32S C32S

canny :: (Cell8U32S e, Format e1) => Int -> (Int, Int) -> DenseMatrix e C1 -> DenseMatrix e1 C1
canny sobel (low, high) = operator11 $ \i (fmt, o) ->
  c'ccv_canny i o fmt (_fi sobel) (_fi low) (_fi high)

otsu :: (Cell8U32S e) => Int -> DenseMatrix e c -> (Int, Double)
otsu range mat = _fi *** _rf $ unsafePerformIO $
  unsafeWithRepr mat $ \pmat ->
  outparam           $ \pd   ->
    c'ccv_otsu pmat pd (_fi range)

opticalFlow :: (Int, Int)
            -> Int
            -> Double
            -> DenseMatrix C8U C1
            -> [(Float, Float)]
            -> DenseMatrix C8U C1
            -> [((Float, Float), Bool)]
opticalFlow _ _ _ _ [] _ = error "optical_flow: empty point set"
opticalFlow win_size level min_eigen mat1 pts mat2 =
  snd $ unsafePerformIO $
  unsafeWithRepr mat1     $ \pmat1 -> 
  unsafeWithRepr mat2     $ \pmat2 -> 
  with (hs2ccv win_size)  $ \pw_s  -> 
  withCCVArray intype pts $ \parr  -> 
  outCCVArray outtype     $ \pparr -> 
    c'_hccv_optical_flow_lucas_kanade pmat1 pmat2 parr pparr pw_s
                                      (_fi level) (_rf min_eigen)
  where
    intype  = Proxy :: Proxy C'ccv_decimal_point_t
    outtype = Proxy :: Proxy C'ccv_decimal_point_with_status_t

(<.>) :: ((a -> c) -> t) -> (b -> c) -> (a -> b) -> t
(<.>) f g h = f (g . h)
infixr 3 <.>


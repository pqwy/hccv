module Basic (
    FlipAxis (..), ColorTransform (..)
  , sobel, gradient, flip, blur, colorTransform
) where

import Prelude hiding ( flip )

import Types
import FfiUtils
import Raw

import Foreign ( Bits (..) )

data FlipAxis = X | Y | XY deriving (Show, Eq) 

instance Flag FlipAxis where
  toFlag X  = c'CCV_FLIP_X
  toFlag Y  = c'CCV_FLIP_Y
  toFlag XY = c'CCV_FLIP_X .|. c'CCV_FLIP_Y

data ColorTransform = RGB_TO_YUV deriving (Show, Eq)

instance Flag ColorTransform where
  toFlag RGB_TO_YUV = c'CCV_RGB_TO_YUV

sobel :: Format (e1 * c) => (Int, Int) -> DenseMatrix e c -> DenseMatrix e1 c
sobel (dx, dy) = operator11 $ \i (fmt, o) ->
  c'ccv_sobel i o fmt (_fi dx) (_fi dy)

gradient :: Format c => (Int, Int) -> DenseMatrix e c -> (DenseMatrix C32F c, DenseMatrix C32F c)
gradient (dx, dy) = operator12 $ \i (_, o1) (_, o2) ->
  c'ccv_gradient i o1 (-666) o2 (-666) (_fi dx) (_fi dy)

flip :: Format (e * c) => FlipAxis -> DenseMatrix e c -> DenseMatrix e c
flip axis = operator11 $ \i (_, o) -> c'ccv_flip i o (-666) (toFlag axis)

blur :: Format (e1 * c) => Double -> DenseMatrix e c -> DenseMatrix e1 c
blur sigma = operator11 $ \i (fmt, o) -> c'ccv_blur i o fmt (realToFrac sigma)

colorTransform :: Format e1 => ColorTransform -> DenseMatrix e C3 -> DenseMatrix e1 C3
colorTransform xf = operator11 $ \i (fmt, o) ->
  c'ccv_color_transform i o fmt (toFlag RGB_TO_YUV)


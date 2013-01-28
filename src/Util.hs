module Util (
    WideCell, FloatCell
  , eq, move, slice, visualize, flatten, shift, any_nan
) where

import Types
import FfiUtils
import Raw

import System.IO.Unsafe


class Format e => WideCell (e :: Cell)
instance WideCell C32S
instance WideCell C32F
instance WideCell C64S
instance WideCell C64F

class Format e => FloatCell (e :: Cell)
instance FloatCell C32F
instance FloatCell C64F


-- Approximate equality.
eq :: DenseMatrix e c -> DenseMatrix e c -> Bool
eq mat1 mat2 = res == 0 where
  res = unsafePerformIO $ unsafeWithRepr mat1 (unsafeWithRepr mat2 . c'ccv_matrix_eq)

move :: Format (e1 * c) => (Int, Int) -> DenseMatrix e c -> DenseMatrix e1 c
move (dx, dy) = operator11 $ \i (fmt, o) ->
  c'ccv_move i o fmt (_fi dx) (_fi dy)

slice :: Format (e1 * c) => (Int, Int) -> (Int, Int) -> DenseMatrix e c -> DenseMatrix e1 c
slice (y, x) (rows, cols) = operator11 $ \i (fmt, o) ->
  c'ccv_slice i o fmt (_fi y) (_fi x) (_fi rows) (_fi cols)

visualize :: DenseMatrix e t -> DenseMatrix C8U C1
visualize = operator11 $ \i (_, o) -> c'ccv_visualize i o (-666)

flatten :: WideCell e1 => DenseMatrix e c -> DenseMatrix e1 C1
flatten = operator11 $ \i (fmt, o) -> c'ccv_flatten i o fmt (-666)

shift :: Format (e1 * c) => Int -> Int -> DenseMatrix e c -> DenseMatrix e1 c
shift lr rr = operator11 $ \i (fmt, o) -> c'ccv_shift i o fmt (_fi lr) (_fi rr)

any_nan :: FloatCell e => DenseMatrix e c -> Bool
any_nan mat = unsafePerformWithRepr mat c'ccv_any_nan == 0


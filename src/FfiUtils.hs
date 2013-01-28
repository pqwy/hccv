module FfiUtils (
  _fi, _rf, operator11, operator12, outparam
) where

import Types
import Raw

import Control.Applicative
import Control.Monad

import Foreign hiding ( unsafePerformIO )
import Foreign.C
import System.IO.Unsafe

_fi :: (Integral a, Num b) => a -> b
_fi = fromIntegral

_rf :: (Real a, Fractional b) => a -> b
_rf = realToFrac

type PtrM = Ptr C'ccv_dense_matrix_t

receive :: Format (e * c) => Ptr (PtrM) -> IO (DenseMatrix e c)
receive = peek >=> newMatrixCCVAlloc

--
-- Wrapper for referentially-transparent matrix->matrix functions that return
-- via outparam. "-g" versions upcast representation to "generic" matrix type.
--

operator11 :: forall e c e1 c1. (Format (e1 * c1))
           => (PtrM -> (CInt, Ptr PtrM) -> IO ())
           -> DenseMatrix e c
           -> DenseMatrix e1 c1
operator11 f mat = unsafePerformIO $
  unsafeWithRepr mat $ \prep  ->
  with nullPtr       $ \ppmat -> do
    f prep (tag (Proxy :: Proxy (e1 * c1)), ppmat)
    receive ppmat

operator12 :: forall e c e1 c1 e2 c2. (Format (e1 * c1), Format (e2 * c2))
            => (PtrM -> (CInt, Ptr PtrM) -> (CInt, Ptr PtrM) -> IO ())
            -> DenseMatrix e c
            -> (DenseMatrix e1 c1, DenseMatrix e2 c2)
operator12 f mat = unsafePerformIO $
  unsafeWithRepr mat $ \prep   ->
  with nullPtr       $ \ppmat1 ->
  with nullPtr       $ \ppmat2 -> do
    f prep (tag (Proxy :: Proxy (e1 * c1)), ppmat1)
           (tag (Proxy :: Proxy (e2 * c2)), ppmat2)
    (,) <$> receive ppmat1 <*> receive ppmat2

-- XXX
-- ccv sometimes tries to reuse outparams when passed (out :: Ptr (Ptr t)) such
-- that (peek out) /= nullPtr . Carefully audit uses of this, it's meant for
-- direct values.
outparam :: Storable a => (Ptr a -> IO b) -> IO (b, a)
outparam f = alloca $ \pa -> (,) <$> f pa <*> peek pa


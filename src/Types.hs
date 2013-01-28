{-# LANGUAGE GADTs  #-}

module Types where

import Raw

import Control.Applicative

import Data.Word

import Control.Exception
import Data.Typeable

import Foreign hiding ( unsafePerformIO )
import Foreign.C ( CInt )

--
import System.IO.Unsafe
import Control.Monad


-- Term-level reification of a non-*-kind type.
data Proxy (t :: k) = Proxy

-- Poly-kinded type pair.
data (a :: k1) * (b :: k2)

-- Types with magic numbers!
class Format (t :: k) where tag :: Proxy t -> CInt

-- Magic numbers "compose" via bitwise-or.
instance (Format a, Format b) => Format (a * b) where
  tag _ = tag (Proxy :: Proxy a) .|. tag (Proxy :: Proxy b)

-- Values with magic numbers!
class Flag (t :: *) where toFlag :: t -> CInt

data Cell = C8U | C32S | C32F | C64S | C64F
  deriving (Eq, Show)

instance Format C8U  where tag _ = c'CCV_8U
instance Format C32S where tag _ = c'CCV_32S
instance Format C32F where tag _ = c'CCV_32F
instance Format C64S where tag _ = c'CCV_64S
instance Format C64F where tag _ = c'CCV_64F

data Chan = C1 | C2 | C3 | C4
  deriving (Eq, Show)

instance Format C1 where tag _ = c'CCV_C1
instance Format C2 where tag _ = c'CCV_C2
instance Format C3 where tag _ = c'CCV_C3
instance Format C4 where tag _ = c'CCV_C4

data MatrixType = MTDense | MTSparse | MTCSR | MTCSC
  deriving (Eq, Show)

instance Format MTDense  where tag _ = c'CCV_MATRIX_DENSE
instance Format MTSparse where tag _ = c'CCV_MATRIX_SPARSE
instance Format MTCSR    where tag _ = c'CCV_MATRIX_CSR
instance Format MTCSC    where tag _ = c'CCV_MATRIX_CSC


--  newtype DenseMatrix (e :: Cell) (c :: Chan) =
--    DenseMatrix (ForeignPtr C'ccv_dense_matrix_t)

data DenseMatrix (e :: Cell) (c :: Chan)
  = DenseMatrixCCVAlloc !(ForeignPtr C'ccv_dense_matrix_t)
  | DenseMatrixOwnAlloc !(ForeignPtr ()) !(ForeignPtr C'ccv_dense_matrix_t)

toForeignPtr :: DenseMatrix e c -> ForeignPtr C'ccv_dense_matrix_t
toForeignPtr (DenseMatrixCCVAlloc   pmat) = pmat
toForeignPtr (DenseMatrixOwnAlloc _ pmat) = pmat

unsafeWithRepr :: DenseMatrix e c -> (Ptr C'ccv_dense_matrix_t -> IO a) -> IO a
unsafeWithRepr = withForeignPtr . toForeignPtr

unsafePerformWithRepr :: DenseMatrix e c -> (Ptr C'ccv_dense_matrix_t -> IO a) -> a
unsafePerformWithRepr = (unsafePerformIO .) . unsafeWithRepr


instance Show (DenseMatrix e c) where
  showsPrec _ m = showString
    $ concat [ "<# DenseMatrix ", show (cols  m)
             , " ["             , show (step  m), "]"
             , " * "            , show (rows  m)
             , " * "            , show (chans m)
             , " >" ]

matrix_type, chans, data_type, cols, rows, step :: DenseMatrix e c -> Int
matrix_type = (`unsafePerformWithRepr` peekIntegral p'ccv_dense_matrix_t'type)
cols        = (`unsafePerformWithRepr` peekIntegral p'ccv_dense_matrix_t'cols)
rows        = (`unsafePerformWithRepr` peekIntegral p'ccv_dense_matrix_t'rows)
step        = (`unsafePerformWithRepr` peekIntegral p'ccv_dense_matrix_t'step)
chans       = m_type_subfield 0xff
data_type   = m_type_subfield 0xff00

m_type_subfield :: Int -> DenseMatrix e c -> Int
m_type_subfield mask = (.&. mask) . matrix_type

matrix_sig :: DenseMatrix e c -> Word64
matrix_sig = (`unsafePerformWithRepr` (peek . p'ccv_dense_matrix_t'sig))

size :: DenseMatrix e c -> (Int, Int)
size mat = (cols mat, rows mat)


assertIO :: Bool -> String -> IO ()
assertIO x err | x         = return ()
               | otherwise = error err

assert_layout :: forall e c. Format (e * c)
              => Proxy (DenseMatrix e c)
              -> Ptr C'ccv_dense_matrix_t
              -> IO ()
assert_layout _ p = do
  have <- (0xffffff .&.) <$> peek (p'ccv_dense_matrix_t'type p)
  assertIO (have == want) $
    concat [ "assert_layout: expected: ", show (layout want)
           , ", got: ", show have, ", ( ", pattern have, " )" ]
  where
    want = tag ( Proxy :: Proxy (e * c * MTDense) )

newMatrixCCVAlloc :: forall e c. Format (e * c)
                   => Ptr C'ccv_dense_matrix_t
                   -> IO (DenseMatrix e c)
newMatrixCCVAlloc p = do
  assert_layout (Proxy :: Proxy (DenseMatrix e c)) p
  DenseMatrixCCVAlloc
    <$> newForeignPtr (castFunPtr p'_hccv_dense_matrix_unref) p

newMatrixOwnAlloc :: forall e c. Format (e * c)
                   => Ptr C'ccv_dense_matrix_t
                   -> ForeignPtr ()
                   -> IO (DenseMatrix e c)
newMatrixOwnAlloc p p' = do
  assert_layout (Proxy :: Proxy (DenseMatrix e c)) p
  DenseMatrixOwnAlloc p' <$>
    newForeignPtr (castFunPtr p'_hccv_dense_matrix_unref) p


newtype CCV_IO_Exn = CCV_IO_Exn Int
  deriving (Eq, Show, Typeable)

instance Exception CCV_IO_Exn where
  toException                     = SomeException
  fromException (SomeException e) = cast e

--

peekIntegral :: (Storable a, Integral a, Num b) => (Ptr t -> Ptr a) -> Ptr t -> IO b
peekIntegral f = (fromIntegral <$>) . peek . f


--

data Merp = Merp {
    tag_    :: CInt
  , format :: Cell
  , chan   :: Chan
  , size_  :: (CInt, CInt)
  , step_  :: CInt
} deriving (Eq, Show)


merp :: Ptr C'ccv_dense_matrix_t -> IO Merp
merp = fmap f . peek where
  f m = Merp {
      tag_   = tag
    , format = f
    , chan   = c
    , size_  = ( c'ccv_dense_matrix_t'cols m
               , c'ccv_dense_matrix_t'rows m )
    , step_  = c'ccv_dense_matrix_t'step m
    }
    where tag      = c'ccv_dense_matrix_t'type m
          ( f, c ) = layout tag


layout :: CInt -> (Cell, Chan)
layout type_ = ( deformat   $ type_ .&. 0xff00
               , dechannels $ type_ .&. 0x00ff )

deformat :: CInt -> Cell
deformat f = assoc f
  [ (c'CCV_8U , C8U )
  , (c'CCV_32S, C32S)
  , (c'CCV_32F, C32F)
  , (c'CCV_64S, C64S)
  , (c'CCV_64F, C64F) ]

dechannels :: CInt -> Chan
dechannels c = assoc c
  [ (c'CCV_C1, C1)
  , (c'CCV_C2, C2)
  , (c'CCV_C3, C3)
  , (c'CCV_C4, C4) ]

assoc :: (Show a, Eq a) => a -> [(a, b)] -> b
assoc a [] = error ("assoc: not found: " ++ show a)
assoc a ((a', b):abs) | a == a'   = b
                      | otherwise = assoc a abs

pattern :: Bits a => a -> String
pattern a = [ f (testBit a i) | i <- [n - 1, n - 2 .. 0] ]
  where
    n = bitSize a
    f False = '.'
    f True  = 'X'

--  instance Eq (DenseMatrix e c) where
--    m1 == m2 = unsafePerformIO $
--      withRepr m1 $ \pm1 ->
--      withRepr m2 $ \pm2 ->
--        if pm1 == pm2 then return True else do
--          (c1, r1, s1, d1) <- mpeek pm1
--          (c2, r2, s2, d2) <- mpeek pm2
--          if c1 /= c2 || r1 /= r2 || s1 /= s2
--             then return False else scan d1 d2 r1 s2
--      where
--        mpeek :: Ptr C'ccv_dense_matrix_t -> IO (CInt, CInt, CInt, Ptr Int32)
--        mpeek p = (,,,) <$> (peek $ p'ccv_dense_matrix_t'cols p)
--                        <*> (peek $ p'ccv_dense_matrix_t'rows p)
--                        <*> (peek $ p'ccv_dense_matrix_t'step p)
--                        <*> (c'ccv_matrix_cell_t'i32 <$>
--                             peek (p'ccv_dense_matrix_t'data p))

--        scan d1 d2 rows step = go d1 d2 0 $ fromIntegral (rows * step)

--          where go :: Ptr Int32 -> Ptr Int32 -> Int -> Int -> IO Bool
--                go p1 p2 i n | i >= n    = return True
--                             | otherwise = do
--                               eq <- (==) <$> (peek $ p1 `plusPtr` i :: IO Int32)
--                                          <*> (peek $ p2 `plusPtr` i :: IO Int32)
--                               if eq then go p1 p2 (i + 4) n else return False

--  eq_array :: forall a. Storable a => Int -> Ptr a -> Ptr a -> IO Bool
--  eq_array bytes p1 p2 = go p1 p2 where
--    elements   = bytes `quot` sizeOf (undefined :: a)
--    (p1', p2') = (p1 `plusPtr` elements, p2 `plusPtr` elements)
--    go n | n >= bytes = return True
--         | otherwise = do
--           x <- peek 

--  eq_region :: Int -> Ptr a -> Ptr a -> IO Bool
--  eq_region bytes p1 p2 |

#include <bindings.dsl.h>
#include <ccv.h>
#include <hccv_helpers.h>

module Raw where
#strict_import


#num CCV_8U
#num CCV_32S
#num CCV_32F
#num CCV_64S
#num CCV_64F

#num CCV_C1
#num CCV_C2
#num CCV_C3
#num CCV_C4

#num CCV_MATRIX_DENSE
#num CCV_MATRIX_SPARSE
#num CCV_MATRIX_CSR
#num CCV_MATRIX_CSC

#num CCV_GARBAGE
#num CCV_REUSABLE
#num CCV_UNMANAGED
#num CCV_NO_DATA_ALLOC

#starttype ccv_matrix_cell_t
#union_field u8  , Ptr CUChar
#union_field i32 , Ptr Int32
#union_field f32 , Ptr CFloat
#union_field i64 , Ptr Int64
#union_field f64 , Ptr CDouble
#stoptype

#starttype ccv_dense_matrix_t
#field type     , CInt
#field sig      , Word64
#field refcount , CInt
#field rows     , CInt
#field cols     , CInt
#field step     , CInt
-- Cutting the corners: if we ever interact with this it's already type-safe,
-- so let's just cut a level of indirection.
-- #field data     , <ccv_matrix_cell_t>
#field data     , Ptr ()
-- tag?
#stoptype

#num CCV_SPARSE_VECTOR
#num CCV_DENSE_VECTOR

#starttype ccv_dense_vector_t
#field step        , CInt
#field length      , CInt
#field index       , CInt
#field prime       , CInt
#field load_factor , CInt
#field data        , <ccv_matrix_cell_t>
#field indice      , Ptr CInt
#field next        , Ptr <ccv_dense_vector_t>
#stoptype

#num CCV_SPARSE_ROW_MAJOR
#num CCV_SPARSE_COL_MAJOR

#starttype ccv_sparse_matrix_t
#field type        , CInt
#field sig         , Word64
#field refcount    , CInt
#field rows        , CInt
#field cols        , CInt
#field major       , CInt
#field prime       , CInt
#field load_factor , CInt
#field vector      , Ptr <ccv_dense_vector_t>
#stoptype

-- #synonym_t ccv_matrix_t , ()
data C'ccv_matrix_t

-- Common personas.
type P_dense   = Ptr C'ccv_dense_matrix_t
type P_p_dense = Ptr (Ptr C'ccv_dense_matrix_t)

-- XXX: A lot of operations are declared as operating on a ccv_matrix_t, but
-- dutifully crash, or simply no-op, if fed the sparse variant.
--
-- These are all described as taking only ccv_dense_vector_t here.

--
-- ccv_cache.c

--
-- ccv_memory.c

-- #ccall ccv_dense_matrix_renew , P_dense -> CInt -> CInt -> CInt -> CInt -> Word64 -> IO P_dense
#ccall ccv_dense_matrix_new   , CInt -> CInt -> CInt -> Ptr () -> Word64 -> IO P_dense
-- #ccall ccv_dense_matrix, CInt -> CInt -> CInt -> Ptr () -> Word64 -> IO P_dense
-- #ccall ccv_make_matrix_mutable     , P_dense -> IO ()
#ccall ccv_make_matrix_immutable   , P_dense -> IO ()
-- #ccall ccv_matrix_free_immediately , P_dense -> IO ()
#ccall ccv_matrix_free , P_dense -> IO ()

#ccall ccv_array_new , CInt -> CInt -> Word64 -> IO (Ptr ccv_array_t)
-- #ccall ccv_make_array_mutable , Ptr <ccv_array_t> -> IO ()
-- #ccall ccv_make_array_immutable , Ptr <ccv_array_t> -> IO ()
#ccall ccv_array_free_immediately , Ptr <ccv_array_t> -> IO ()
#ccall ccv_array_free , Ptr <ccv_array_t> -> IO ()

#ccall ccv_drain_cache , IO ()
#ccall ccv_disable_cache , IO ()
#ccall ccv_enable_cache , CSize -> IO ()
#ccall ccv_enable_default_cache , IO ()

--
-- ccv_io.c

#num CCV_IO_GRAY
#num CCV_IO_RGB_COLOR

#num CCV_IO_NO_COPY

#num CCV_IO_ANY_STREAM
#num CCV_IO_BMP_STREAM
#num CCV_IO_JPEG_STREAM
#num CCV_IO_PNG_STREAM
#num CCV_IO_PLAIN_STREAM
#num CCV_IO_DEFLATE_STREAM

#num CCV_IO_ANY_FILE
#num CCV_IO_BMP_FILE
#num CCV_IO_JPEG_FILE
#num CCV_IO_PNG_FILE
#num CCV_IO_BINARY_FILE

#num CCV_IO_ANY_RAW
#num CCV_IO_RGB_RAW
#num CCV_IO_RGBA_RAW
#num CCV_IO_ARGB_RAW
#num CCV_IO_BGR_RAW
#num CCV_IO_BGRA_RAW
#num CCV_IO_ABGR_RAW
#num CCV_IO_GRAY_RAW

#num CCV_IO_FINAL
#num CCV_IO_CONTINUE
#num CCV_IO_ERROR
#num CCV_IO_ATTEMPTED
#num CCV_IO_UNKNOWN

#ccall ccv_read_impl , Ptr () -> P_p_dense -> CInt -> CInt -> CInt -> CInt -> IO CInt
#ccall ccv_write , P_dense -> Ptr CChar -> Ptr CInt -> CInt -> Ptr () -> IO CInt

--
-- ccv_algebra.c

--
-- ccv_util.c

-- #ccall ccv_get_dense_matrix , Ptr <ccv_matrix_t>
-- #ccall ccv_get_sparse_prime , Ptr <ccv_matrix_t>
-- #ccall ccv_get_sparse_matrix_vector , Ptr <ccv_sparse_matrix_t> -> CInt -> IO (Ptr <ccv_dense_vector_t>)
-- #ccall ccv_get_sparse_matrix_cell   , Ptr <ccv_sparse_matrix_t> -> CInt -> CInt -> IO <ccv_matrix_cell_t>
-- #ccall ccv_set_sparse_matrix_cell , Ptr <ccv_sparse_matrix_t> -> CInt -> CInt -> Ptr () -> IO ()
-- #ccall ccv_compress_sparse_matrix , Ptr <ccv_sparse_matrix_t> -> Ptr (Pr <ccv_compressed_sparse_matrix_t>) -> IO ()
-- #ccall ccv_decompress_sparse_matrix , Ptr <ccv_compressed_sparse_matrix_t> -> Ptr (Ptr <ccv_sparse_matrix_t>) -> IO ()

#ccall ccv_array_push  , Ptr <ccv_array_t> -> Ptr a -> IO ()
#ccall ccv_array_clear , Ptr <ccv_array_t> -> IO ()


#ccall ccv_move      , P_dense -> P_p_dense -> CInt -> CInt -> CInt -> IO ()
#ccall ccv_matrix_eq , P_dense -> P_dense -> IO CInt
#ccall ccv_slice     , P_dense -> P_p_dense -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
#ccall ccv_visualize , P_dense -> P_p_dense -> CInt -> IO ()
#ccall ccv_flatten   , P_dense -> P_p_dense -> CInt -> CInt -> IO ()
-- #ccall ccv_zero      , P_dense -> IO ()
#ccall ccv_shift     , P_dense -> P_p_dense -> CInt -> CInt -> CInt -> IO ()
#ccall ccv_any_nan   , P_dense -> IO CInt

#starttype ccv_size_t
#field width  , CInt
#field height , CInt
#stoptype

#starttype ccv_rect_t
#field x      , CInt
#field y      , CInt
#field width  , CInt
#field height , CInt
#stoptype

#starttype ccv_array_t
#field type     , CInt
#field sig      , Word64
#field refcount , CInt
#field rnum     , CInt
#field size     , CInt
#field rsize    , CInt
#field data     , Ptr ()
#stoptype

#starttype ccv_point_t
#field x , CInt
#field y , CInt
#stoptype

#starttype ccv_decimal_point_t
#field x , CFloat
#field y , CFloat
#stoptype

#starttype ccv_decimal_point_with_status_t
#field point  , <ccv_decimal_point_t>
#field status , Word8
#stoptype

#starttype ccv_contour_t
#field rect , <ccv_rect_t>
#field size , CInt
#field set  , Ptr <ccv_array_t>
#field m10  , CLong
#field m01  , CLong
#field m11  , CLong
#field m20  , CLong
#field m02  , CLong
#stoptype

--
-- ccv_numeric.c

--
-- ccv_basic.c

#num CCV_FLIP_X
#num CCV_FLIP_Y

#num CCV_RGB_TO_YUV

#ccall ccv_sobel           , P_dense -> P_p_dense -> CInt -> CInt -> CInt -> IO ()
#ccall ccv_gradient        , P_dense -> P_p_dense -> CInt -> P_p_dense -> CInt -> CInt -> CInt -> IO ()
#ccall ccv_flip            , P_dense -> P_p_dense -> CInt -> CInt -> IO ()
#ccall ccv_blur            , P_dense -> P_p_dense -> CInt -> CDouble -> IO ()
#ccall ccv_color_transform , P_dense -> P_p_dense -> CInt -> CInt -> IO ()

--
-- ccv_resample.c

--
-- ccv_transform.c

--
-- ccv_classic.c

#ccall ccv_canny , P_dense -> P_p_dense -> CInt -> CInt -> CInt -> CInt -> IO ()
#ccall ccv_otsu  , P_dense -> Ptr CDouble -> CInt -> IO CInt
#ccall _hccv_optical_flow_lucas_kanade ,           \
  P_dense -> P_dense                               \
  -> Ptr <ccv_array_t> -> Ptr (Ptr <ccv_array_t>)  \
  -> Ptr <ccv_size_t> -> CInt -> CDouble           \
  -> IO ()


--
-- helpers

#ccall _hccv_dense_matrix_ref    , P_dense -> IO ()
#ccall _hccv_dense_matrix_unref  , P_dense -> IO ()
#ccall _hccv_dense_matrix_unref2 , P_dense -> Ptr a -> IO ()

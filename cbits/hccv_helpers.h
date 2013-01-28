#include <ccv.h>

void _hccv_optical_flow_lucas_kanade (
    ccv_dense_matrix_t* a, ccv_dense_matrix_t* b,
    ccv_array_t*  pa, ccv_array_t** pb,
    ccv_size_t* ws,
    int level, double min_eigen
  );

void _hccv_dense_matrix_ref (ccv_dense_matrix_t *mat);
void _hccv_dense_matrix_unref (ccv_dense_matrix_t *mat);
void _hccv_dense_matrix_unref2 (ccv_dense_matrix_t *mat, void *x);

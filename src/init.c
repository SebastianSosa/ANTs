#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _ANTs_assoc_mat(SEXP, SEXP);
extern SEXP _ANTs_assoc_mat_one_id(SEXP, SEXP, SEXP);
extern SEXP _ANTs_ComplexEigen(SEXP);
extern SEXP _ANTs_df_merge(SEXP, SEXP);
extern SEXP _ANTs_df_to_gbi(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_edgl_to_matrix(SEXP, SEXP);
extern SEXP _ANTs_euclidean(SEXP);
extern SEXP _ANTs_fastLm_Impl(SEXP, SEXP, SEXP);
extern SEXP _ANTs_gbi_createEmpty(SEXP, SEXP, SEXP);
extern SEXP _ANTs_ldf_merge(SEXP);
extern SEXP _ANTs_list_lapply(SEXP, SEXP);
extern SEXP _ANTs_list_to_df(SEXP);
extern SEXP _ANTs_listDf_merge_single_column(SEXP, SEXP, SEXP);
extern SEXP _ANTs_mat_binaryzation(SEXP);
extern SEXP _ANTs_mat_col_extract(SEXP, SEXP);
extern SEXP _ANTs_mat_col_sumsBinary(SEXP);
extern SEXP _ANTs_mat_cols_sums(SEXP);
extern SEXP _ANTs_mat_dim(SEXP);
extern SEXP _ANTs_mat_erase_col(SEXP, SEXP);
extern SEXP _ANTs_mat_fill_upper(SEXP);
extern SEXP _ANTs_mat_filter(SEXP, SEXP, SEXP);
extern SEXP _ANTs_mat_find0(SEXP);
extern SEXP _ANTs_mat_isSquare(SEXP);
extern SEXP _ANTs_mat_lowertri_to_vec(SEXP, SEXP);
extern SEXP _ANTs_mat_row_extract(SEXP, SEXP);
extern SEXP _ANTs_mat_row_wise_multiplication(SEXP, SEXP);
extern SEXP _ANTs_mat_rows_sums(SEXP);
extern SEXP _ANTs_mat_rows_sumsBinary(SEXP);
extern SEXP _ANTs_mat_sym_by_lowertri(SEXP);
extern SEXP _ANTs_mat_symetrization(SEXP);
extern SEXP _ANTs_mat_to_vec(SEXP, SEXP);
extern SEXP _ANTs_met_assor_cat(SEXP, SEXP);
extern SEXP _ANTs_met_cc(SEXP, SEXP);
extern SEXP _ANTs_met_degree(SEXP);
extern SEXP _ANTs_met_density(SEXP);
extern SEXP _ANTs_met_ei(SEXP);
extern SEXP _ANTs_met_ei2(SEXP);
extern SEXP _ANTs_met_eigen(SEXP, SEXP, SEXP);
extern SEXP _ANTs_met_strength(SEXP);
extern SEXP _ANTs_met_sum_egos_strength(SEXP);
extern SEXP _ANTs_metric_global_shortestDetails(SEXP);
extern SEXP _ANTs_metric_global_shortestDetailsBasedBetween(SEXP);
extern SEXP _ANTs_metric_global_shortestPath(SEXP);
extern SEXP _ANTs_metric_global_triangle(SEXP);
extern SEXP _ANTs_metric_node_betweeness(SEXP);
extern SEXP _ANTs_na_omit(SEXP);
extern SEXP _ANTs_perm_dataStream_ControlFactor(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_perm_dataStream1(SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_perm_dataStream1_focal(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_perm_mat_col_row(SEXP, SEXP, SEXP);
extern SEXP _ANTs_perm_mat_row_col(SEXP, SEXP, SEXP);
extern SEXP _ANTs_perm_matVec(SEXP, SEXP, SEXP);
extern SEXP _ANTs_perm_nl_rf(SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_perm_nodeLabels(SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_perm_vec_factor(SEXP);
extern SEXP _ANTs_perm_vec_int(SEXP, SEXP, SEXP);
extern SEXP _ANTs_perm_net_weigths(SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_redo_perm_dataStream_1(SEXP, SEXP, SEXP);
extern SEXP _ANTs_redo_perm_dataStream_ControlFactor(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_redo_perm_dataStream_ControlFactor_scd(SEXP, SEXP, SEXP);
extern SEXP _ANTs_redo_perm_dataStream_focal(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_redo_perm_dataStream1_focal(SEXP, SEXP, SEXP, SEXP);
extern SEXP _ANTs_stat_chol2inv(SEXP);
extern SEXP _ANTs_stat_t_value(SEXP, SEXP);
extern SEXP _ANTs_test_arma(SEXP);
extern SEXP _ANTs_test_const_arma(SEXP);
extern SEXP _ANTs_test_nm(SEXP);
extern SEXP _ANTs_test_nm_conv(SEXP);
extern SEXP _ANTs_tobs_to_mat(SEXP);
extern SEXP _ANTs_vec_char_as_factor(SEXP);
extern SEXP _ANTs_vec_char_extract_IdValue(SEXP, SEXP);
extern SEXP _ANTs_vec_id_Equal0(SEXP);
extern SEXP _ANTs_vec_id_sup0(SEXP);
extern SEXP _ANTs_vec_intersect(SEXP, SEXP);
extern SEXP _ANTs_vec_levels(SEXP);
extern SEXP _ANTs_vec_lowertri_to_mat(SEXP, SEXP, SEXP);
extern SEXP _ANTs_vec_match(SEXP, SEXP);
extern SEXP _ANTs_vec_merge(SEXP, SEXP);
extern SEXP _ANTs_vec_fill(SEXP, SEXP, SEXP);
extern SEXP _ANTs_vec_num_extract(SEXP, SEXP);
extern SEXP _ANTs_vec_num_extract_IdValue(SEXP, SEXP);
extern SEXP _ANTs_vec_sample(SEXP, SEXP, SEXP);
extern SEXP _ANTs_vec_sample_all(SEXP);
extern SEXP _ANTs_vec_sum(SEXP);
extern SEXP _ANTs_vec_to_mat(SEXP, SEXP);
extern SEXP _ANTs_vec_to_mat_add_diag(SEXP, SEXP);
extern SEXP _ANTs_vec_unique(SEXP);
extern SEXP _ANTs_vec_unmatch(SEXP, SEXP);
extern SEXP _ANTs_vec_unmatch_indexcc(SEXP, SEXP);
extern SEXP _ANTs_vec_vec_multiply(SEXP, SEXP);
extern SEXP _ANTs_vec_vec_sum(SEXP, SEXP);
extern SEXP _ANTs_vector_abs(SEXP);
extern SEXP _ANTs_which_equal(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_ANTs_assoc_mat",                                 (DL_FUNC) &_ANTs_assoc_mat,                                 2},
  {"_ANTs_assoc_mat_one_id",                          (DL_FUNC) &_ANTs_assoc_mat_one_id,                          3},
  {"_ANTs_ComplexEigen",                              (DL_FUNC) &_ANTs_ComplexEigen,                              1},
  {"_ANTs_df_merge",                                  (DL_FUNC) &_ANTs_df_merge,                                  2},
  {"_ANTs_df_to_gbi",                                 (DL_FUNC) &_ANTs_df_to_gbi,                                 5},
  {"_ANTs_edgl_to_matrix",                            (DL_FUNC) &_ANTs_edgl_to_matrix,                            2},
  {"_ANTs_euclidean",                                 (DL_FUNC) &_ANTs_euclidean,                                 1},
  {"_ANTs_fastLm_Impl",                               (DL_FUNC) &_ANTs_fastLm_Impl,                               3},
  {"_ANTs_gbi_createEmpty",                           (DL_FUNC) &_ANTs_gbi_createEmpty,                           3},
  {"_ANTs_ldf_merge",                                 (DL_FUNC) &_ANTs_ldf_merge,                                 1},
  {"_ANTs_list_lapply",                               (DL_FUNC) &_ANTs_list_lapply,                               2},
  {"_ANTs_list_to_df",                                (DL_FUNC) &_ANTs_list_to_df,                                1},
  {"_ANTs_listDf_merge_single_column",                (DL_FUNC) &_ANTs_listDf_merge_single_column,                3},
  {"_ANTs_mat_binaryzation",                          (DL_FUNC) &_ANTs_mat_binaryzation,                          1},
  {"_ANTs_mat_col_extract",                           (DL_FUNC) &_ANTs_mat_col_extract,                           2},
  {"_ANTs_mat_col_sumsBinary",                        (DL_FUNC) &_ANTs_mat_col_sumsBinary,                        1},
  {"_ANTs_mat_cols_sums",                             (DL_FUNC) &_ANTs_mat_cols_sums,                             1},
  {"_ANTs_mat_dim",                                   (DL_FUNC) &_ANTs_mat_dim,                                   1},
  {"_ANTs_mat_erase_col",                             (DL_FUNC) &_ANTs_mat_erase_col,                             2},
  {"_ANTs_mat_fill_upper",                            (DL_FUNC) &_ANTs_mat_fill_upper,                            1},
  {"_ANTs_mat_filter",                                (DL_FUNC) &_ANTs_mat_filter,                                3},
  {"_ANTs_mat_find0",                                 (DL_FUNC) &_ANTs_mat_find0,                                 1},
  {"_ANTs_mat_isSquare",                              (DL_FUNC) &_ANTs_mat_isSquare,                              1},
  {"_ANTs_mat_lowertri_to_vec",                       (DL_FUNC) &_ANTs_mat_lowertri_to_vec,                       2},
  {"_ANTs_mat_row_extract",                           (DL_FUNC) &_ANTs_mat_row_extract,                           2},
  {"_ANTs_mat_row_wise_multiplication",               (DL_FUNC) &_ANTs_mat_row_wise_multiplication,               2},
  {"_ANTs_mat_rows_sums",                             (DL_FUNC) &_ANTs_mat_rows_sums,                             1},
  {"_ANTs_mat_rows_sumsBinary",                       (DL_FUNC) &_ANTs_mat_rows_sumsBinary,                       1},
  {"_ANTs_mat_sym_by_lowertri",                       (DL_FUNC) &_ANTs_mat_sym_by_lowertri,                       1},
  {"_ANTs_mat_symetrization",                         (DL_FUNC) &_ANTs_mat_symetrization,                         1},
  {"_ANTs_mat_to_vec",                                (DL_FUNC) &_ANTs_mat_to_vec,                                2},
  {"_ANTs_met_assor_cat",                             (DL_FUNC) &_ANTs_met_assor_cat,                             2},
  {"_ANTs_met_cc",                                    (DL_FUNC) &_ANTs_met_cc,                                    2},
  {"_ANTs_met_degree",                                (DL_FUNC) &_ANTs_met_degree,                                1},
  {"_ANTs_met_density",                               (DL_FUNC) &_ANTs_met_density,                               1},
  {"_ANTs_met_ei",                                    (DL_FUNC) &_ANTs_met_ei,                                    1},
  {"_ANTs_met_ei2",                                   (DL_FUNC) &_ANTs_met_ei2,                                   1},
  {"_ANTs_met_eigen",                                 (DL_FUNC) &_ANTs_met_eigen,                                 3},
  {"_ANTs_met_strength",                              (DL_FUNC) &_ANTs_met_strength,                              1},
  {"_ANTs_met_sum_egos_strength",                     (DL_FUNC) &_ANTs_met_sum_egos_strength,                     1},
  {"_ANTs_metric_global_shortestDetails",             (DL_FUNC) &_ANTs_metric_global_shortestDetails,             1},
  {"_ANTs_metric_global_shortestDetailsBasedBetween", (DL_FUNC) &_ANTs_metric_global_shortestDetailsBasedBetween, 1},
  {"_ANTs_metric_global_shortestPath",                (DL_FUNC) &_ANTs_metric_global_shortestPath,                1},
  {"_ANTs_metric_global_triangle",                    (DL_FUNC) &_ANTs_metric_global_triangle,                    1},
  {"_ANTs_metric_node_betweeness",                    (DL_FUNC) &_ANTs_metric_node_betweeness,                    1},
  {"_ANTs_na_omit",                                   (DL_FUNC) &_ANTs_na_omit,                                   1},
  {"_ANTs_perm_dataStream_ControlFactor",             (DL_FUNC) &_ANTs_perm_dataStream_ControlFactor,             6},
  {"_ANTs_perm_dataStream1",                          (DL_FUNC) &_ANTs_perm_dataStream1,                          4},
  {"_ANTs_perm_dataStream1_focal",                    (DL_FUNC) &_ANTs_perm_dataStream1_focal,                    5},
  {"_ANTs_perm_mat_col_row",                          (DL_FUNC) &_ANTs_perm_mat_col_row,                          3},
  {"_ANTs_perm_mat_row_col",                          (DL_FUNC) &_ANTs_perm_mat_row_col,                          3},
  {"_ANTs_perm_matVec",                               (DL_FUNC) &_ANTs_perm_matVec,                               3},
  {"_ANTs_perm_nl_rf",                                (DL_FUNC) &_ANTs_perm_nl_rf,                                4},
  {"_ANTs_perm_nodeLabels",                           (DL_FUNC) &_ANTs_perm_nodeLabels,                           4},
  {"_ANTs_perm_vec_factor",                           (DL_FUNC) &_ANTs_perm_vec_factor,                           1},
  {"_ANTs_perm_vec_int",                              (DL_FUNC) &_ANTs_perm_vec_int,                              3},
  {"_ANTs_perm_net_weigths",                          (DL_FUNC) &_ANTs_perm_net_weigths,                          4},
  {"_ANTs_redo_perm_dataStream_1",                    (DL_FUNC) &_ANTs_redo_perm_dataStream_1,                    3},
  {"_ANTs_redo_perm_dataStream_ControlFactor",        (DL_FUNC) &_ANTs_redo_perm_dataStream_ControlFactor,        5},
  {"_ANTs_redo_perm_dataStream_ControlFactor_scd",    (DL_FUNC) &_ANTs_redo_perm_dataStream_ControlFactor_scd,    3},
  {"_ANTs_redo_perm_dataStream_focal",                (DL_FUNC) &_ANTs_redo_perm_dataStream_focal,                6},
  {"_ANTs_redo_perm_dataStream1_focal",               (DL_FUNC) &_ANTs_redo_perm_dataStream1_focal,               4},
  {"_ANTs_stat_chol2inv",                             (DL_FUNC) &_ANTs_stat_chol2inv,                             1},
  {"_ANTs_stat_t_value",                              (DL_FUNC) &_ANTs_stat_t_value,                              2},
  {"_ANTs_test_arma",                                 (DL_FUNC) &_ANTs_test_arma,                                 1},
  {"_ANTs_test_const_arma",                           (DL_FUNC) &_ANTs_test_const_arma,                           1},
  {"_ANTs_test_nm",                                   (DL_FUNC) &_ANTs_test_nm,                                   1},
  {"_ANTs_test_nm_conv",                              (DL_FUNC) &_ANTs_test_nm_conv,                              1},
  {"_ANTs_tobs_to_mat",                               (DL_FUNC) &_ANTs_tobs_to_mat,                               1},
  {"_ANTs_vec_char_as_factor",                        (DL_FUNC) &_ANTs_vec_char_as_factor,                        1},
  {"_ANTs_vec_char_extract_IdValue",                  (DL_FUNC) &_ANTs_vec_char_extract_IdValue,                  2},
  {"_ANTs_vec_id_Equal0",                             (DL_FUNC) &_ANTs_vec_id_Equal0,                             1},
  {"_ANTs_vec_id_sup0",                               (DL_FUNC) &_ANTs_vec_id_sup0,                               1},
  {"_ANTs_vec_intersect",                             (DL_FUNC) &_ANTs_vec_intersect,                             2},
  {"_ANTs_vec_levels",                                (DL_FUNC) &_ANTs_vec_levels,                                1},
  {"_ANTs_vec_lowertri_to_mat",                       (DL_FUNC) &_ANTs_vec_lowertri_to_mat,                       3},
  {"_ANTs_vec_match",                                 (DL_FUNC) &_ANTs_vec_match,                                 2},
  {"_ANTs_vec_merge",                                 (DL_FUNC) &_ANTs_vec_merge,                                 2},
  {"_ANTs_vec_num_extract",                           (DL_FUNC) &_ANTs_vec_num_extract,                           2},
  {"_ANTs_vec_num_extract_IdValue",                   (DL_FUNC) &_ANTs_vec_num_extract_IdValue,                   2},
  {"_ANTs_vec_sample",                                (DL_FUNC) &_ANTs_vec_sample,                                3},
  {"_ANTs_vec_sample_all",                            (DL_FUNC) &_ANTs_vec_sample_all,                            1},
  {"_ANTs_vec_sum",                                   (DL_FUNC) &_ANTs_vec_sum,                                   1},
  {"_ANTs_vec_to_mat",                                (DL_FUNC) &_ANTs_vec_to_mat,                                2},
  {"_ANTs_vec_to_mat_add_diag",                       (DL_FUNC) &_ANTs_vec_to_mat_add_diag,                       2},
  {"_ANTs_vec_unique",                                (DL_FUNC) &_ANTs_vec_unique,                                1},
  {"_ANTs_vec_unmatch",                               (DL_FUNC) &_ANTs_vec_unmatch,                               2},
  {"_ANTs_vec_unmatch_indexcc",                       (DL_FUNC) &_ANTs_vec_unmatch_indexcc,                       2},
  {"_ANTs_vec_vec_multiply",                          (DL_FUNC) &_ANTs_vec_vec_multiply,                          2},
  {"_ANTs_vec_vec_sum",                               (DL_FUNC) &_ANTs_vec_vec_sum,                               2},
  {"_ANTs_vector_abs",                                (DL_FUNC) &_ANTs_vector_abs,                                1},
  {"_ANTs_which_equal",                               (DL_FUNC) &_ANTs_which_equal,                               2},
  {NULL, NULL, 0}
};

void R_init_ant(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// find_infected_neighbors_rcpp
int find_infected_neighbors_rcpp(IntegerMatrix infection_matrix, int i, int j);
RcppExport SEXP _furvin_find_infected_neighbors_rcpp(SEXP infection_matrixSEXP, SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type infection_matrix(infection_matrixSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(find_infected_neighbors_rcpp(infection_matrix, i, j));
    return rcpp_result_gen;
END_RCPP
}
// forest_fire_rcpp
IntegerMatrix forest_fire_rcpp(IntegerMatrix infection_matrix, double alpha, double beta, bool pausing);
RcppExport SEXP _furvin_forest_fire_rcpp(SEXP infection_matrixSEXP, SEXP alphaSEXP, SEXP betaSEXP, SEXP pausingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type infection_matrix(infection_matrixSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< bool >::type pausing(pausingSEXP);
    rcpp_result_gen = Rcpp::wrap(forest_fire_rcpp(infection_matrix, alpha, beta, pausing));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_furvin_find_infected_neighbors_rcpp", (DL_FUNC) &_furvin_find_infected_neighbors_rcpp, 3},
    {"_furvin_forest_fire_rcpp", (DL_FUNC) &_furvin_forest_fire_rcpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_furvin(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

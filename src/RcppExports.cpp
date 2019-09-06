// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// wle
IntegerVector wle(NumericVector counts, int activity_threshold, int spike_tolerance, int spike_stoplevel);
RcppExport SEXP _accelR_wle(SEXP countsSEXP, SEXP activity_thresholdSEXP, SEXP spike_toleranceSEXP, SEXP spike_stoplevelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type counts(countsSEXP);
    Rcpp::traits::input_parameter< int >::type activity_threshold(activity_thresholdSEXP);
    Rcpp::traits::input_parameter< int >::type spike_tolerance(spike_toleranceSEXP);
    Rcpp::traits::input_parameter< int >::type spike_stoplevel(spike_stoplevelSEXP);
    rcpp_result_gen = Rcpp::wrap(wle(counts, activity_threshold, spike_tolerance, spike_stoplevel));
    return rcpp_result_gen;
END_RCPP
}
// overlap
LogicalVector overlap(IntegerVector start, IntegerVector end);
RcppExport SEXP _accelR_overlap(SEXP startSEXP, SEXP endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type end(endSEXP);
    rcpp_result_gen = Rcpp::wrap(overlap(start, end));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_accelR_wle", (DL_FUNC) &_accelR_wle, 4},
    {"_accelR_overlap", (DL_FUNC) &_accelR_overlap, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_accelR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
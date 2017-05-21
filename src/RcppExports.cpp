// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// activation_function
arma::vec activation_function(arma::vec activation, arma::vec input, double decay, double floor, double ceiling);
RcppExport SEXP PCSinR_activation_function(SEXP activationSEXP, SEXP inputSEXP, SEXP decaySEXP, SEXP floorSEXP, SEXP ceilingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type activation(activationSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type input(inputSEXP);
    Rcpp::traits::input_parameter< double >::type decay(decaySEXP);
    Rcpp::traits::input_parameter< double >::type floor(floorSEXP);
    Rcpp::traits::input_parameter< double >::type ceiling(ceilingSEXP);
    rcpp_result_gen = Rcpp::wrap(activation_function(activation, input, decay, floor, ceiling));
    return rcpp_result_gen;
END_RCPP
}
// update_activation
arma::vec update_activation(arma::vec activation, arma::mat weights, double decay, double floor, double ceiling);
RcppExport SEXP PCSinR_update_activation(SEXP activationSEXP, SEXP weightsSEXP, SEXP decaySEXP, SEXP floorSEXP, SEXP ceilingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type activation(activationSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< double >::type decay(decaySEXP);
    Rcpp::traits::input_parameter< double >::type floor(floorSEXP);
    Rcpp::traits::input_parameter< double >::type ceiling(ceilingSEXP);
    rcpp_result_gen = Rcpp::wrap(update_activation(activation, weights, decay, floor, ceiling));
    return rcpp_result_gen;
END_RCPP
}
// check_convergence
bool check_convergence(arma::vec energy, int iter, double stability, String convergence);
RcppExport SEXP PCSinR_check_convergence(SEXP energySEXP, SEXP iterSEXP, SEXP stabilitySEXP, SEXP convergenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type energy(energySEXP);
    Rcpp::traits::input_parameter< int >::type iter(iterSEXP);
    Rcpp::traits::input_parameter< double >::type stability(stabilitySEXP);
    Rcpp::traits::input_parameter< String >::type convergence(convergenceSEXP);
    rcpp_result_gen = Rcpp::wrap(check_convergence(energy, iter, stability, convergence));
    return rcpp_result_gen;
END_RCPP
}
// convergence_floor
bool convergence_floor(arma::vec energy, int iter, double stability);
RcppExport SEXP PCSinR_convergence_floor(SEXP energySEXP, SEXP iterSEXP, SEXP stabilitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type energy(energySEXP);
    Rcpp::traits::input_parameter< int >::type iter(iterSEXP);
    Rcpp::traits::input_parameter< double >::type stability(stabilitySEXP);
    rcpp_result_gen = Rcpp::wrap(convergence_floor(energy, iter, stability));
    return rcpp_result_gen;
END_RCPP
}
// pcs_matrix_cpp
Rcpp::List pcs_matrix_cpp(arma::mat weights, arma::vec start, arma::vec reset, double decay, int maxiter, double stability, String convergence, bool full);
RcppExport SEXP PCSinR_pcs_matrix_cpp(SEXP weightsSEXP, SEXP startSEXP, SEXP resetSEXP, SEXP decaySEXP, SEXP maxiterSEXP, SEXP stabilitySEXP, SEXP convergenceSEXP, SEXP fullSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type start(startSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type reset(resetSEXP);
    Rcpp::traits::input_parameter< double >::type decay(decaySEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< double >::type stability(stabilitySEXP);
    Rcpp::traits::input_parameter< String >::type convergence(convergenceSEXP);
    Rcpp::traits::input_parameter< bool >::type full(fullSEXP);
    rcpp_result_gen = Rcpp::wrap(pcs_matrix_cpp(weights, start, reset, decay, maxiter, stability, convergence, full));
    return rcpp_result_gen;
END_RCPP
}
// timesTwo
NumericVector timesTwo(NumericVector x);
RcppExport SEXP PCSinR_timesTwo(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(timesTwo(x));
    return rcpp_result_gen;
END_RCPP
}

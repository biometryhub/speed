#define RCPP_ARMADILLO_RETURN_ANYVEC_AS_VECTOR
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


// Cols ----------------

    // [[Rcpp::export]]
arma::rowvec Arma_colSums(const arma::mat& x) {
    return arma::sum(x, 0);
}

// [[Rcpp::export]]
NumericVector Sugar_colSums(const NumericMatrix& x) {
    return colSums(x);
}

// [[Rcpp::export]]
NumericVector Cpp_colSums(const NumericMatrix& x) {
    int nr = x.nrow(), nc = x.ncol();
    NumericVector ans(nc);
    for (int j = 0; j < nc; j++) {
        double sum = 0.0;
        for (int i = 0; i < nr; i++) {
            sum += x(i, j);
        }
        ans[j] = sum;
    }
    return ans;
}


// Rows ----------------

    // [[Rcpp::export]]
arma::colvec Arma_rowSums(const arma::mat& x) {
    return arma::sum(x, 1);
}

// [[Rcpp::export]]
NumericVector Sugar_rowSums(const NumericMatrix& x) {
    return rowSums(x);
}

// [[Rcpp::export]]
NumericVector Cpp_rowSums(const NumericMatrix& x) {
    int nr = x.nrow(), nc = x.ncol();
    NumericVector ans(nr);
    for (int j = 0; j < nc; j++) {
        for (int i = 0; i < nr; i++) {
            ans[i] += x(i, j);
        }
    }
    return ans;
}

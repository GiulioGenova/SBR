#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]                                                             
NumericVector cumsumBounded(NumericVector x, double low, double high) {
  NumericVector res(x.size());
  double acc = 0;
  for (int i=0; i < x.size(); ++i) {
    acc += x[i];
    if (acc < low)  acc = low;
    else if (acc > high)  acc = high;
    res[i] = acc;
  }
  return res;
}
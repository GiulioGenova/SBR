#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector cumsumBounded(NumericVector x, double low, double high,double acc) {
  NumericVector res(x.size());
  for (int i=0; i < x.size(); ++i) {
    acc += x[i];
    if (acc < low)  acc = low;
    else if (acc > high)  acc = high;
    res[i] = acc;
  }
  return res;
}

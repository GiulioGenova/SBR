#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector wbadj(NumericVector n, NumericVector e, double taw, double p, NumericVector k, double dry) {
  NumericVector etcadj(e.size());
  double raw = taw*p;
  double acc = 0;
  double ks = 0;
  for (int i=0; i < n.size(); ++i) {
    //acc = acc + e[i] - n[i];
    if ((acc + e[i] - n[i]) <= 0) {
      acc = 0;
    }
    else if ((acc + e[i] - n[i])> 0 & (acc + e[i] - n[i]) < raw){
      acc = acc + e[i]*k[i] - n[i];
    }
    else if ((acc + e[i] - n[i])> raw & (acc + e[i] - n[i]) < dry) {
      ks = (taw-(acc + e[i] - n[i])) / ((1 - p) * taw );
      acc = acc + e[i]*k[i]*ks - n[i];
    }
    else {
      acc = dry ;
    }
    etcadj[i] = acc;
  }
  return etcadj;
}

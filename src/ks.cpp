#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector ks(NumericVector n, NumericVector e, NumericVector irrig , double taw, double p, NumericVector k) {
  NumericVector ksout(e.size());
  double raw = taw*p;
  double acc = 0;
  double ks = 0;
  for (int i=0; i < n.size(); ++i) {
    //acc = acc + e[i] - n[i];
    if ((acc + e[i]*k[i] - n[i] - irrig[i]) <= 0.00) {
      acc = 0;
      ks=1;
    }
    else if ((acc + e[i]*k[i] - n[i] - irrig[i])> 0.00 & (acc + e[i]*k[i] - n[i] - irrig[i]) < raw){
      acc = acc + e[i]*k[i] - n[i] - irrig[i];
      ks=1;
    }
    else if ((acc + e[i]*k[i] - n[i] - irrig[i])> raw & (acc + e[i]*k[i] - n[i] - irrig[i]) < taw) {
      ks = (taw-(acc + e[i]*k[i] - n[i] - irrig[i])) / ((1 - p) * taw );
      acc = acc + e[i]*k[i]*ks - n[i] - irrig[i];
    }
    else {
      acc = taw ;
      ks = (taw-acc) / ((1 - p) * taw );
    }
    ksout[i] = ks;
  }
  return ksout;
}

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector wbadj(NumericVector n, NumericVector e, NumericVector irrig, double taw, double p, NumericVector k,double acc) {
  NumericVector etcadj(e.size());
  double raw = taw*p;
  double ks = 0;
  for (int i=0; i < n.size(); ++i) {
    //acc = acc + e[i] - n[i];
    if ((acc + e[i]*k[i] - n[i] - irrig[i]) <= 0.00) {
      acc = 0;
    }
    else if ((acc + e[i]*k[i] - n[i] - irrig[i])> 0.00 & (acc + e[i]*k[i] - n[i] - irrig[i]) < raw){
      acc = acc + e[i]*k[i] - n[i] - irrig[i];
    }
    else if ((acc + e[i]*k[i] - n[i] - irrig[i])> raw & (acc + e[i]*k[i] - n[i] - irrig[i]) < taw) {
      ks = (taw-(acc + e[i]*k[i] - n[i] - irrig[i])) / ((1 - p) * taw );
      acc = acc + e[i]*k[i]*ks - n[i] - irrig[i];
    }
    else {
      acc = taw ;
    }
    etcadj[i] = acc;
  }
  return etcadj;
}

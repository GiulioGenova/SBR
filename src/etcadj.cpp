#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector etcadj(NumericVector n, NumericVector e, NumericVector irrig ,double taw, double p, NumericVector k) {
  NumericVector etcadjout(e.size());
  double raw = taw*p;
  double acc = 0;
  double ks = 0;
  double etcadj = 0;
  for (int i=0; i < n.size(); ++i) {
    //acc = acc + e[i] - n[i];
    if ((acc + e[i]*k[i] - n[i] - irrig[i]) <= 0) {
      acc = 0;
      ks=1;
      etcadj = e[i]*k[i]*ks;
    }
    else if ((acc + e[i]*k[i] - n[i] - irrig[i])> 0 & (acc + e[i]*k[i] - n[i] - irrig[i]) <= raw){
      acc = acc + e[i]*k[i] - n[i] - irrig[i];
      ks=1;
      etcadj = e[i]*k[i]*ks;
    }
    else if ((acc + e[i]*k[i] - n[i] - irrig[i])> raw & (acc + e[i]*k[i] - n[i] - irrig[i]) < taw) {
      ks = (taw-(acc + e[i]*k[i] - n[i] - irrig[i])) / ((1 - p) * taw );
      acc = acc + e[i]*k[i]*ks - n[i] - irrig[i];
      etcadj = e[i]*k[i]*ks;
    }
    else {
      acc = taw ;
      ks = (taw-acc) / ((1 - p) * taw );
      etcadj = e[i]*k[i]*ks;
    }
    etcadjout[i] = etcadj;
  }
  return etcadjout;
}

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector waste_water_adj(NumericVector n, NumericVector e, NumericVector irrig, double taw, double p, NumericVector k,double acc) {
  NumericVector waste_water(e.size());
  double raw = taw*p;
  double ks = 0;
  double waste = 0;
  double need = 0;
  for (int i=0; i < n.size(); ++i) {
    //acc = acc + e[i] - n[i];
    if ((acc + e[i]*k[i] - n[i] - irrig[i]) <= 0) {
      if ((-acc - e[i]*k[i] + n[i]) > 0){
        waste = irrig[i];
      }
      else if ((-acc - e[i]*k[i] + n[i]) <= 0) {
        need = acc + e[i]*k[i] - n[i];
        waste = irrig[i] - need;
      }
      acc = 0;
    }
    else if ((acc + e[i]*k[i] - n[i] - irrig[i])> 0 & (acc + e[i]*k[i] - n[i] - irrig[i]) < raw){
      acc = acc + e[i]*k[i] - n[i] - irrig[i];
      waste = 0;
    }
    else if ((acc + e[i]*k[i] - n[i] - irrig[i])> raw & (acc + e[i]*k[i] - n[i] - irrig[i]) < taw) {
      ks = (taw-(acc + e[i]*k[i] - n[i] - irrig[i])) / ((1 - p) * taw );
      acc = acc + e[i]*k[i]*ks - n[i] - irrig[i];
      waste = 0;
    }
    else {
      acc = taw ;
      waste = 0;
    }
    waste_water[i] = waste;
  }
  return waste_water;
}

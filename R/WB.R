#' Computes computes water balace from evapotranspiration and precipitation data coming from et function
#' and getForecast function
#'
#' @export
#' @importFrom dplyr row_number
#' @importFrom Rcpp sourceCpp
#' @useDynLib SBR

WB <- function(data,min_mm=10,max_mm=50,lmitWarning_mm=30,lmitStress_mm=20){

  data <- data %>%
    mutate(
      N_sumMinusETc=ifelse(row_number()==1,max_mm, N_sum-ETc),
      wb=ifelse(row_number()==1,max_mm,cumsumBounded(x = N_sumMinusETc,
                                                     low = min_mm,
                                                     high = max_mm)),
      irrigAdvise=ifelse(wb<=max_mm & wb> lmitWarning_mm,"NoIrrig",
                         ifelse(wb<= lmitWarning_mm & wb> lmitStress_mm,"SugIrrig","MustIrrig")),
      mmToIrrig=ifelse(wb<lmitWarning_mm,max_mm-wb,0),
      wbadj=ifelse(row_number()==1,0,etcadj(n=N_sum,e = ET0,
                                            taw = 50,p = 0.5,
                                            k = Kc,dry = 100
      )),
      ks=ifelse(row_number()==1,0,ks(n=N_sum,e = ET0,
                                     taw = 50,p = 0.5,
                                     k = Kc,dry = 100
      ))
    )

}

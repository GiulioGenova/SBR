#' Computes computes water balace from evapotranspiration and precipitation data coming from et function
#' and getForecast function
#'
#' @export
#' @importFrom dplyr row_number
#' @importFrom Rcpp sourceCpp
#' @useDynLib SBR

WB <- function(data,min_mm=10,max_mm=50){

  data <- data %>%
    mutate(
      N_sumMinusETc=ifelse(row_number()==1,max_mm, N_sum-ETc),
      wb=ifelse(row_number()==1,max_mm,cumsumBounded(x = N_sumMinusETc,
                                                     low = min_mm,
                                                     high = max_mm)))

}

#' Computes computes water balace from evapotranspiration and precipitation data coming from ET function,
#' mergeOldAndForecast function and getForecast function
#'
#' @export
#' @importFrom dplyr row_number mutate
#' @importFrom Rcpp sourceCpp
#' @useDynLib SBR
#' @param data a table output of SBR::ET or SBR::mergeOldAndForecast or SBR::getForecast functions
#' @param taw the total available water. Default 50
#' @param lmitWarning fraction of the taw below which the app suggests to irrigate. Default 0.8
#' @param p fraction of the taw defining the raw (readly available water)
#' @param startwb is the initial water balance. Default value is NULL which yields taw*p
#' @param irrig vector with irrigation. Same lenght as rows of data or one number for the starting irrigation (first day of the time series)
#'

WB <- function(data,taw=50,lmitWarning=0.8,p=0.5,startwb= NULL,irrig=0){

  if(!any(names(data)=="irrig")){

    if(length(irrig)==1){

      data$irrig <- c(irrig,rep(0,nrow(data)-1))

    }else{

      data$irrig <- irrig

    }
  }

  if(is.null(startwb)) {startwb=taw*p}

  data <- data %>%
    mutate(
      N_sum=ifelse(is.na(N_sum),0,N_sum),

      wbnotadj=cumsumBounded(x = ETc-N_sum - irrig,
                             low = 0,high = taw,acc=startwb),

      irrigAdvisenotadj=ifelse(wbnotadj<=taw*p*lmitWarning,
                               "NoIrrig",ifelse(wbnotadj>= taw*p*lmitWarning
                                                & wbnotadj< taw*p,
                                                "SugIrrig","MustIrrig")),

      etcadj=ifelse(is.na(ET0),ETc,
                    etcadj(n=N_sum, irrig = irrig ,e = ET0,taw = taw,
                           p = p,k = Kc,acc=startwb)),

      ks=ks(n=N_sum, irrig = irrig ,e = ET0,taw = taw,
            p = p,k = Kc,acc=startwb),

      wb=ifelse(is.na(ET0),
                cumsumBounded(x = ETc-N_sum - irrig,
                              low = 0,high = taw,acc=startwb),
                wbadj(n=N_sum, irrig = irrig ,e = ET0,taw = taw,
                      p = p,k = Kc,acc=startwb)),

      waste=waste_water_adj(n=N_sum,e = ET0,irrig=irrig,taw = taw,
                            p = p,k = Kc,acc=startwb),

      irrigAdvise=ifelse(wb<=taw*p*lmitWarning,"NoIrrig",
                         ifelse(wb>= taw*p*lmitWarning & wb< taw*p,
                                "SugIrrig","MustIrrig")),

      mmToIrrig=ifelse(wb<taw*p*lmitWarning,0,wb)

    )

}

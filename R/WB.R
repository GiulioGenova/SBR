#' Computes computes water balace from evapotranspiration and precipitation data coming from et function
#' and getForecast function
#'
#' @export
#' @importFrom dplyr row_number
#' @importFrom Rcpp sourceCpp
#' @useDynLib SBR

WB <- function(data,taw=50,lmitWarning=0.7,p=0.5,startwb= 10){

  data <- data %>%
    mutate(
      N_sum=ifelse(row_number()==1,startwb,N_sum),
      #ETcMinusN_sum=ifelse(row_number()==1,start, ETc-N_sum),

      wbnotadj=cumsumBounded(x = ETc-N_sum,low = 0,high = taw),

      # etcadj=ifelse(row_number()==1,start,etcadj(n=N_sum,e = ET0,
      #                                            taw = taw,p = p,
      #                                            k = Kc
      # )),



      etcadj=etcadj(n=N_sum,e = ET0,taw = taw,p = p,k = Kc
      ),

      # ks=ifelse(row_number()==1,0,ks(n=N_sum,e = ET0,
      #                                taw = taw,p = p,
      #                                k = Kc
      # )),
      ks=ks(n=N_sum,e = ET0,taw = taw,p = p,k = Kc
      ),


      # wb=ifelse(row_number()==1,start,wbadj(n=N_sum,e = ET0,
      #                                       taw = taw,p = p,
      #                                       k = Kc
      # )),
      wb=wbadj(n=N_sum,e = ET0,taw = taw,p = p,k = Kc
      ),


      irrigAdvise=ifelse(wb<=taw*p*lmitWarning,"NoIrrig",
                         ifelse(wb>= taw*p*lmitWarning & wb< taw*p,"SugIrrig","MustIrrig")),

      mmToIrrig=ifelse(wb<taw*p*lmitWarning,0,wb)


    )

}

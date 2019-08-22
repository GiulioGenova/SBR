#' merge et and precipitation data from wheater forecast and dayly measured et data
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate yday
#' @importFrom dplyr mutate select left_join bind_rows
#'


mergeOldAndForecast <- function(data,long,lat,taw=50,lmitWarning=0.8,p=0.5,slope=NULL){

  dff<-mergeEtAndForecast(long = long,lat = lat,slope=slope)

  colnames(dff)[colnames(dff)=="ET"] <- "ETc"

  dfo<- data %>% select(TimeStamp,N_sum,ETc,wb,irrigAdvise,mmToIrrig) %>%
    filter(TimeStamp<Sys.Date())


  start=dfo$wb[nrow(dfo)]
  if(nrow(dfo)==0) start=0

  dff <- dff %>%
    mutate(

      ETcMinusN_sum=ifelse(row_number()==1,start+ETc-N_sum, ETc-N_sum),

      wb=ifelse(row_number()==1,start+ETc-N_sum,cumsumBounded(x = ETcMinusN_sum,
                                                              low = 0,
                                                              high = taw)),

      irrigAdvise=ifelse(wb<=taw*p*lmitWarning,"NoIrrig",
                         ifelse(wb>= taw*p*lmitWarning & wb< taw*p,"SugIrrig","MustIrrig")),

      mmToIrrig=ifelse(wb<taw*p*lmitWarning,0,wb)


    )

  df<-bind_rows(dfo,dff)
  return(df)
}

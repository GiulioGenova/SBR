#' Get data from province of Bozen sensors. Internal function
#'
#' @export
#' @importFrom MonalisR getMeteoInfo
#' @import data.table
#'
get_provBz_sensors <- function(){

  se<-unique(as.data.table(MonalisR::getMeteoInfo()), by = c("SCODE", "TYPE"))
  return(se)

}

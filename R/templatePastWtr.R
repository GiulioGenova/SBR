#' create an html template for the past weather
#'
#' @export

templatePastWtr <- function(rain,temperatureMax,temperatureMin) {
  sprintf(
    '<div>Niederschlag [mm]:</div>
     <div><b>%s</b></div>
     <div>Temperatur [°C]:</div>
     <div>max <b>%s</b> min <b>%s</b></div>
    '
    ,
    rain,temperatureMax,temperatureMin
  )
}

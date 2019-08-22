#' create an html template for image
#'
#' @export

templateImg <- function(img,width,height,rainFrom,rainTo,temperatureMax,temperatureMin) {
  sprintf(
    '<div><img src="%s" width="%s" height="%s"></div>
     <div>Niederschlag [mm]:</div>
     <div>von <b>%s</b> bis <b>%s</b></div>
     <div>Temperatur [°C]:</div>
     <div>max <b>%s</b> min <b>%s</b></div>
    '
    ,
    img,width,height,rainFrom,rainTo,temperatureMax,temperatureMin
  )
}

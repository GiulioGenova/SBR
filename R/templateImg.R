#' create an html template for image
#'
#' @export

templateImg <- function(img,width,height,rainFrom,rainTo,temperatureMax,temperatureMin) {
  sprintf(
    '<div><img src="%s" width="%s" height="%s"></div>
     <div>Niederschlag [mm]:</div>
     <div>von %s bis %s</div>
     <div>Temperatur [°C]:</div>
     <div>max %s min %s</div>
    '
    ,
    img,width,height,rainFrom,rainTo,temperatureMax,temperatureMin
  )
}

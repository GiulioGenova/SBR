#' create an html template for image
#'
#' @export

templateImg <- function(img,width,height) {
  sprintf(
    '<img src="%s" width="%s" height="%s">',
    img,width,height
  )
}

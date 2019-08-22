#' plot the data of irrigation advice
#'
#' @export
#' @import  timevis
#' @import  htmltools
#' @importFrom dplyr mutate case_when
#' @importFrom tidyr gather
#'

plotIrrigAdvice2 <- function(db, wthrIcns=T){

  MustIrrig="Bewässern<br>notwendig"
  NoIrrig="Keine<br>Bewässerung"
  SugIrrig="Bewässerung<br>empfohlen"

  db <- db %>% dplyr::mutate(
    irrigAdvise = dplyr::case_when(
      irrigAdvise=="MustIrrig" ~ MustIrrig,
      irrigAdvise=="NoIrrig" ~ NoIrrig,
      irrigAdvise=="SugIrrig" ~ SugIrrig
    ),
    img=SBR::templateImg(img=img,width=50,height=50,rainFrom,rainTo,temperatureMax,temperatureMin)
  )


  db <- tidyr::gather(db,"key","value",irrigAdvise,img)

  db <- db %>% dplyr::mutate(
    style = dplyr::case_when(
      value==MustIrrig ~"background-color: #cc3232;font-weight: bold; font-size: 105%;",
      value==NoIrrig ~ "background-color: #2dc937;font-weight: bold; font-size: 105%;",
      value==SugIrrig ~ "background-color: #db7b2b;font-weight: bold; font-size: 105%;"
    ),
    color = dplyr::case_when(
      value==MustIrrig ~ "#cc3232",
      value==NoIrrig ~ "#2dc937",
      value==SugIrrig ~ "#db7b2b"
    )
  )



  data <- data.frame(
    id      = 1:nrow(db),
    content = db$value,
    start   = paste(db$TimeStamp, "01:00"),
    end     = paste(db$TimeStamp, "23:00"),
    #style   = paste("background-color:", db$color ,";","font-weight: bold; font-size: 105%;"),
    style   = db$style,
    group   = db$key

  )

  p<-timevis(data,groups = data.frame(id = unique(db$key),
                                      content = unique(
                                        ifelse(db$key=="irrigAdvise",
                                               "Bewässerung",
                                               "Wetter")
                                      )),
             options = list(locale = 'de',stack=FALSE)#
  )

  return(p)
}

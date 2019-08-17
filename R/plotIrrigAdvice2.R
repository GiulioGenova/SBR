#' plot the data of irrigation advice
#'
#' @export
#' @import  timevis
#' @import  htmltools
#' @importFrom dplyr mutate case_when
#' @importFrom tidyr gather
#'

plotIrrigAdvice2 <- function(db, wthrIcns=T){

  MustIrrig="Muss<br>bewässern"
  NoIrrig="Keinen<br>bewässerung"
  SugIrrig="Bewässerung<br>nvorschlagen"

  db <- db %>% dplyr::mutate(
    irrigAdvise = dplyr::case_when(
      irrigAdvise=="MustIrrig" ~ MustIrrig,
      irrigAdvise=="NoIrrig" ~ NoIrrig,
      irrigAdvise=="SugIrrig" ~ SugIrrig
    ),
    img=SBR::templateImg(img=img,width=50,height=50)
  )


  db <- tidyr::gather(db,"key","value",irrigAdvise,img)

  db <- db %>% dplyr::mutate(

    color = dplyr::case_when(
      value==MustIrrig ~ "red",
      value==NoIrrig ~ "green",
      value==SugIrrig ~ "orange"
    )
  )



  data <- data.frame(
    id      = 1:nrow(db),
    content = db$value,
    start   = paste(db$TimeStamp, "01:00"),
    end     = paste(db$TimeStamp, "23:00"),
    style   = paste("background-color:", db$color ,";"),
    group   = db$key

  )

  p<-timevis(data,groups = data.frame(id = unique(db$key),
                                      content = unique(
                                        ifelse(db$key=="irrigAdvise",
                                               "Bewässerung",
                                               "Wetter")
                                      ))
  )

  return(p)
}

#' plot the data of irrigation advice
#'
#' @export
#' @import  ggplot2
#' @importFrom dplyr mutate
#'

plotIrrigAdvice <- function(db, wthrIcns=T){

  # ggplot2::ggplot(db(),aes(x=TimeStamp,y=wb,color=irrigAdvise))+
  #   geom_line()+
  #   #geom_rect(aes(xmin = min(TimeStamp), xmax = max(TimeStamp),
  #   #              ymin = -Inf, ymax = Inf, fill = irrigAdvise), alpha = 0.4)+
  #   geom_hline(yintercept=0,size=50,color=irrigAdvise)


  #db$img<-"http://daten.buergernetz.bz.it/services/weather/graphics/icons/imgsource/wetter/icon_18.png"

  MustIrrig="Muss\nbewässern"
  NoIrrig="Keine\nbewässerung"
  SugIrrig="Bewässerung\nvorschlagen"

  fill_values=c("Muss\nbewässern"="red", "Keine\nbewässerung"="green","Bewässerung\nvorschlagen"="orange")

  db <- db %>% mutate(
    TimeStamp=substr(TimeStamp,6,10),
    TimeStamp = factor(TimeStamp),
    TimeStamp = factor(TimeStamp,
                       levels = rev(levels(TimeStamp))),
    irrigAdvise = case_when(
      irrigAdvise=="MustIrrig" ~ MustIrrig,
      irrigAdvise=="NoIrrig" ~ NoIrrig,
      irrigAdvise=="SugIrrig" ~ SugIrrig
    ))

  p <- ggplot(db, aes(y=TimeStamp)) +#
    labs(x=NULL,y=NULL,fill=NULL)+
    geom_tile(aes(x="1",fill=irrigAdvise,width=0.8),#hjust = 0.1,, height=1
              color="white",show.legend = T,size=1.5)#
  #coord_fixed(ratio = 1)
  if (wthrIcns){p<- p + geom_image(x = 1.6, aes(image = img),size=.15) }

  #coord_flip() +
  #expand_limits(x = c(1,1.1))  +
  p  + coord_cartesian(xlim = c(0.98, 2),expand = F)+
    #scale_y_reverse()+
    scale_fill_manual(values=fill_values)+
    #coord_flip()+
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_blank(),#element_line(linetype = "dashed",colour = "grey"),
          axis.text.x=element_blank(),
          axis.text.y=element_text(face = "plain",size = 20,colour = "black"),
          axis.title.y = element_blank(),
          axis.title.x =  element_blank(),
          #axis.title.x = element_text(size = axis_title_size),#,margin = margin(r=20,l=20)
          legend.position = "top",#,

          # legend.title = element_text(size=legend_text_size),
          legend.text = element_text(size = 13)
          # legend.key = element_rect(colour = "#f1fafc"),#size = 2
          # legend.key.size = unit(3, 'lines'),
          # axis.line.x = element_line(colour = "black",size=1.2)
    )


}

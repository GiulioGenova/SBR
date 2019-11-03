#' Uses the output from water_balance_by_station and creates formats and writes an excell file .xlsx
#'
#' @export
#' @importFrom xlsx createWorkbook createSheet CellBlock CB.setRowData CB.setRowData Font CB.setFont CB.setBorder Fill CB.setFill saveWorkbook Border
#' @importFrom lubridate month day
#' @importFrom dplyr select mutate_if rename
#' @param df a dataframe as output form water_balance_by_station
#' @param filepath the absolute path of the xlsx to write. has to include extension .xlsx
#' @param trgtM a date from wich the function derives the abbreviation of the name in german. Used to name the excell sheet
#'

createAndFormatXlsx = function(df,filepath,trgtM = Sys.Date()){

  wb = createWorkbook()
  month=as.character(month(trgtM,label = T,locale = "German",abbr = F))
  sheet  = createSheet(wb, sheetName=month)
  datum_names <- day(df[[1]]$TimeStamp)

  colnames=c("Name","Messungen",datum_names,"Total")

  cellBlockMonth = CellBlock(sheet, 1, 3, 1, 1)
  CB.setRowData(cellBlock = cellBlockMonth, x = month, rowIndex = 1)


  cellBlockHeader = CellBlock(sheet, 2, 1, 1, length(colnames))
  CB.setRowData(cellBlock = cellBlockHeader, x = colnames, rowIndex = 1)



  font=Font(wb = wb,heightInPoints= 12,isBold = T)
  CB.setFont(cellBlock = cellBlockHeader, font, rowIndex=rep(1,length(colnames)),
             colIndex = 1:length(colnames))

  CB.setFont(cellBlock = cellBlockMonth, font, rowIndex=1,
             colIndex = 1)

  for (i in 1:length(df)) {

    x=df[[i]]

    x = x %>% select(name,N_sum,ETc,irrigAdvisenotadj)%>%
      mutate_if(is.numeric, ~round(.,2))

    x <- x %>%
      rename(Name=name,`Niederschläge`=N_sum,Verdunstung=ETc
      )

    total=c("",sum(x$`Niederschläge`),sum(x$Verdunstung),"")
    stat_name=c(unique(x$Name),"","","")
    x$Name=""
    colnames(x)[which(colnames(x)=="Name")]<-""
    x=rbind(stat_name,colnames(x),x,total)
    x=t(x)


    red_col=which(x["irrigAdvisenotadj",]=="MustIrrig")
    orange_col=which(x["irrigAdvisenotadj",]=="SugIrrig")
    green_col=which(x["irrigAdvisenotadj",]=="NoIrrig")

    x = x[-which(row.names(x)=="irrigAdvisenotadj"),]
    x[,1] = sub("_"," ",x[,1])
    x[,1] = sub("_"," ",x[,1])

    cellBlock = CellBlock(sheet, (i*nrow(x)), 1, nrow(x), ncol(x))


    border_left=Border(color="black", position="LEFT", pen="BORDER_THIN")
    CB.setBorder(cellBlock,border = border_left,rowIndex= sort(rep(1:nrow(x),ncol(x))),
                 colIndex= rep(1:ncol(x),nrow(x)))

    border_top=Border(color="black", position="TOP", pen="BORDER_THICK")
    CB.setBorder(cellBlock,border = border_top,rowIndex= rep(1,ncol(x)),
                 colIndex= rep(1:ncol(x),1))

    if((i %% 2) != 0) {

      fill_grey <- Fill(foregroundColor = "#e2e9ef",
                        backgroundColor="#e2e9ef")

      CB.setFill(cellBlock, fill_grey, rowIndex= sort(rep(1:nrow(x),ncol(x))),
                 colIndex= rep(1:ncol(x),nrow(x)))
    }

    for (y in 1:nrow(x)) {

      CB.setRowData(cellBlock = cellBlock, x = x[y,], rowIndex = y)

      if(row.names(x)[y]=="Verdunstung"){

        fill_red <- Fill(foregroundColor = "#cc3232", backgroundColor="#cc3232")
        CB.setFill( cellBlock, fill_red, rowIndex= rep(y,length(red_col)),
                    colIndex= red_col)

        fill_orange <- Fill(foregroundColor = "#db7b2b", backgroundColor="#db7b2b")
        CB.setFill( cellBlock, fill_orange, rowIndex= rep(y,length(orange_col)),
                    colIndex= orange_col)

        fill_green <- Fill(foregroundColor = "#2dc937", backgroundColor="#2dc937")
        CB.setFill( cellBlock, fill_green, rowIndex= rep(y,length(green_col)),
                    colIndex= green_col)
      }

    }

  }

  saveWorkbook(wb, filepath)

}

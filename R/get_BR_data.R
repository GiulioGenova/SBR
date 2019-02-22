#' Get data from SBR monitoring stations.
#'
#' @param station station number.
#' @param datestart starting date of the timeseries.
#' @param dateend ending date of the timeseries.
#' @export
#' @importFrom lubridate as_datetime year floor_date
#' @importFrom RMariaDB MariaDB
#' @importFrom tidyr gather
#' @importFrom dplyr left_join mutate bind_rows
#' @importFrom DBI dbGetQuery dbConnect dbDisconnect
#' @importFrom magrittr %>%



get_BR_data <- function(station,
                        sensor=NULL,
                        datestart=Sys.Date()-1,dateend=Sys.Date()+1,
                        user,
                        password,
                        host,
                        add_names=FALSE,
                        spread=FALSE,
                        round="hour"){

  dateend=as_date(dateend)+1


  #station=c(9,103,35)
  tab <- sensor_file %>%
    filter(StationsNr%in%(station))



  if(round=="raw"){
    #sensor=c("Trockentemperatur 60cm","Temperatur 2m","Feuchte max in Messinterval")

    if(!is.null(sensor)){
      sensor=unique(sensor_file[sensor_file$MesswertBezDe%in%sensor,"MesswertNr"])
      tab <- tab %>% filter(MesswertNr%in%(sensor)) %>%
        select(messwertSpalte,Faktor,MesswertBezDe,MesswertEh) %>%
        unique

    }


    sensors = as.character(
      paste0(tab$messwertSpalte,
             "/",tab$Faktor,
             " AS ","'",
             tab$MesswertBezDe,
             "'",
             collapse=", "))

  }else {


    if(!is.null(sensor)){

      #sensor=c("Trockentemperatur 60cm_avg","Temperatur 2m_avg","Feuchte max in Messinterval_min")
      sensor_avg=sensor[grep(sensor,pattern = "*_avg")]
      sensor_avg=sub("\\_.*", "", sensor_avg)
      sensor_avg=unique(sensor_file[sensor_file$MesswertBezDe%in%sensor_avg,"MesswertNr"])

      tab_avg <- tab %>% filter(MesswertNr%in%(sensor_avg)) %>%
        select(messwertSpalte,Faktor,MesswertBezDe,MesswertEh) %>%
        unique

      if(nrow(tab_avg)>0){
        query_avg = paste0(
          "avg(",
          tab_avg$messwertSpalte,
          ")",
          "/",tab_avg$Faktor,
          " AS ","'",
          tab_avg$MesswertBezDe,
          "_avg",
          "'",
          collapse=", ")

      }else{query_avg=NULL}

      sensor_sum=sensor[grep(sensor,pattern = "*_sum")]
      sensor_sum=sub("\\_.*", "", sensor_sum)
      sensor_sum=unique(sensor_file[sensor_file$MesswertBezDe%in%sensor_sum,"MesswertNr"])

      tab_sum <- tab %>% filter(MesswertNr%in%(sensor_sum)) %>%
        select(messwertSpalte,Faktor,MesswertBezDe,MesswertEh) %>%
        unique

      if(nrow(tab_sum)>0){
        query_sum = paste0(
          "sum(",
          tab_sum$messwertSpalte,
          ")",
          "/",tab_sum$Faktor,
          " AS ","'",
          tab_sum$MesswertBezDe,
          "_sum",
          "'",
          collapse=", ")

      }else{query_sum=NULL}

      sensor_min=sensor[grep(sensor,pattern = "*_min")]
      sensor_min=sub("\\_.*", "", sensor_min)
      sensor_min=unique(sensor_file[sensor_file$MesswertBezDe%in%sensor_min,"MesswertNr"])

      tab_min <- tab %>% filter(MesswertNr%in%(sensor_min)) %>%
        select(messwertSpalte,Faktor,MesswertBezDe,MesswertEh) %>%
        unique

      if(nrow(tab_min)>0){
        query_min = paste0(
          "min(",
          tab_min$messwertSpalte,
          ")",
          "/",tab_min$Faktor,
          " AS ","'",
          tab_min$MesswertBezDe,
          "_min",
          "'",
          collapse=", ")

      }else{query_min=NULL}


      sensor_max=sensor[grep(sensor,pattern = "*_max")]
      sensor_max=sub("\\_.*", "", sensor_max)
      sensor_max=unique(sensor_file[sensor_file$MesswertBezDe%in%sensor_max,"MesswertNr"])

      tab_max <- tab %>% filter(MesswertNr%in%(sensor_max)) %>%
        select(messwertSpalte,Faktor,MesswertBezDe,MesswertEh) %>%
        unique

      if(nrow(tab_max)>0){

        query_max = paste0(
          "max(",
          tab_max$messwertSpalte,
          ")",
          "/",tab_max$Faktor,
          " AS ","'",
          tab_max$MesswertBezDe,
          "_max",
          "'",
          collapse=", ")

      }else{query_max=NULL}

    }

    query=c(query_avg,query_sum,query_min,query_max)

    sensors = as.character(
      paste(collapse =", ",#sep = ",",
            query
      )

    )
  }


  query_SBR_data <- function(year,station,sensors,datestart,
                             dateend,user,password,host){
    #year=2018;year=2013
    #station=c(7,12,14,172,176)
    con <- dbConnect(MariaDB(),#RMariaDB::
                     dbname = sprintf('sbr_wetter_%s',year),
                     host = host,user =  user,
                     password = password)#

    #Value10         AS 'Irrig_On_Off', \
    #Value14 / 10    AS 'GS', \
    #Value11 / 10    AS 'ET', \
    #Value7          AS 'WR', \

    if(year==2013){
      com1="#"
      com2="#"
    }else {
      com1=""
      com2=""
    }

    if(round=="raw"){



      # Value3 / 10     AS 'LF', \
      # Value4 / 10     AS 'LT', \
      # Value6 / 10     AS 'WG', \
      #
      # Value9 / 10     AS 'N', \
      # Value10         AS 'IRYN', \
      #
      #
      # Value20 / 10    AS 'IR', \
      # BF10 / 1000     AS 'BWC20', \
      # BF30 / 1000     AS 'BWC40', \
      # %s BT10 / 10       AS 'BT20', \
      # %s BT30 / 10       AS 'BT40', \
      # BF50 / 10       AS 'BWP20', \
      # BF80 / 10       AS 'BWP40'\


      query<-sprintf("SELECT \
                 Datum           AS 'TimeStamp', \
                 StationsNr      AS 'id' , \
                 %s \
                 FROM \
                 tab_messung \
                 WHERE \
                 (StationsNr =%s) AND Datum >= '%s' AND Datum <= '%s'",
                     #com1,com2,
                     sensors,
                     as.character(paste(station,collapse=" OR StationsNr =")),
                     datestart,dateend)#
    }else{

      if(round == "year"){
        roundExpr <- "DATE_FORMAT(Datum, '%Y-01-01')"
      }else if (round == "month"){
        roundExpr <- "DATE_FORMAT(Datum, '%Y-%m-01')"
      }else if (round == "day"){
        roundExpr <- "date(Datum)"
      }else if(round == "hour"){
        roundExpr <- "CONCAT_WS(' ',DATE_FORMAT(Datum, '%Y-%m-%d'),time_format(Datum,'%H:00:00'))"
      }


      # min(Value3 / 10)     AS 'LF_min', \
      # avg(Value3 / 10)     AS 'LF_mean',\
      # max(Value3 / 10)     AS 'LF_max', \
      #
      # min(Value4 / 10)     AS 'LT_min', \
      # avg(Value4 / 10)     AS 'LT_mean',\
      # max(Value4 / 10)     AS 'LT_max', \
      #
      # avg(Value6 / 10 )    AS 'WG_mean', \
      #
      # sum(Value9 / 10)     AS 'N_sum', \
      # sum(Value10/12)      AS 'IRYN_h', \
      # sum(Value20 / 10)    AS 'IR_sum', \
      #
      # avg(BF10 / 1000)     AS 'BWC20_mean', \
      # avg(BF30 / 1000)     AS 'BWC40_mean', \
      # %s avg(BT10 / 10)       AS 'BT20_mean', \
      # %s avg(BT30 / 10)       AS 'BT40_mean', \
      # avg(BF50 / 10)       AS 'BWP20_mean', \
      # avg(BF80 / 10)       AS 'BWP40_mean'\



      query<-sprintf("SELECT \
                     %s           AS 'TimeStamp', \
                     StationsNr      AS 'id' , \
                     %s \
                     FROM \
                     tab_messung \
                     WHERE \
                     (StationsNr =%s) AND Datum >= '%s' AND Datum <= '%s'\
                     group by %s,id",roundExpr,sensors,#com1,com2,
                     as.character(paste(station,collapse=" OR StationsNr =")),
                     datestart,dateend,roundExpr)#


    }
    #AND Datum >= '%s' AND Datum <= '%s'
    #mysql> SELECT * FROM pet WHERE species = 'dog' AND sex = 'f';
    #x<-tbl(con,dbListTables(con))
    #y<-as.data.frame()
    #x[["ops"]]$vars
    #x$StationsNr
    query
    db <- dbGetQuery(con, query)
    dbDisconnect(con)

    if(year==2013){
      if(round=="raw"){
        db$BT20=NA
        db$BT40=NA
      }else {
        db$BT20_mean=NA
        db$BT40_mean=NA
      }
    }
    #db<-db%>%filter(date>= startdate & date<= enddate)#%>%
    db
  }

  years <- as.character(seq(as.numeric(year(datestart)),
                            as.numeric(year(dateend),1)))

  db <- lapply(years,query_SBR_data,station=station,
               datestart=datestart,
               dateend=dateend,user=user,
               password=password,host=host,
               sensors=sensors)#,round=input$round

  db <- bind_rows(db)

  if(round=="raw"){
    round <- "5 min"}

  db <- db %>%
    filter(TimeStamp < dateend)

  db$TimeStamp <- as_datetime(db$TimeStamp,tz="Europe/Berlin") %>%
    floor_date(round)

  if(!spread){

    db <- db %>%
      gather(Sensor,Value,-TimeStamp,-id)#tidyr::

    if(round=="5 min"){
      db <-full_join(
        db,tab %>% select(MesswertBezDe,MesswertEh) ,
        by = c("Sensor"="MesswertBezDe")
      )
    }

  }

  if(add_names){

    db <- left_join(db,name_file)

  }

  return(db)

}

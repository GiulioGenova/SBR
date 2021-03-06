#' Computes Evapotraspiration using PenmanMonteith from data retrieved with function mergeData
#'
#' @export
#' @importFrom lubridate year month day yday
#' @importFrom dplyr mutate select bind_cols left_join
#' @importFrom Evapotranspiration ReadInputs
#'



ET <- function(data,
               Kc.ini.A=0.1,
               Kc.ini.B=0.4,
               Kc.dev=0.7,
               Kc.mid=1.2,
               Kc.late=0.8,
               DOY.ini.A=69,
               DOY.ini.B=71,
               DOY.dev=91,
               DOY.mid=141,
               DOY.late=271,
               DOY.harv=312,
               tree_height=3,
               slope=NULL,
               radiation_unit="W",
               crop = "short",
               netRadiation="no",
               wind = "yes",
               message = "yes",
               latitude=NULL,
               Elev=NULL
){

  if(radiation_unit=="W"){
    cnvrt=0.0864
  }else if(radiation_unit=="MJ"){
    cnvrt=1
  }


  colnames(data)[colnames(data)=="GS_mean"] <- "Rs"
  colnames(data)[colnames(data)=="LF_min"] <- "RHmin"
  colnames(data)[colnames(data)=="LF_max"] <- "RHmax"
  colnames(data)[colnames(data)=="LT_max"] <- "Tmax"
  colnames(data)[colnames(data)=="LT_min"] <- "Tmin"
  colnames(data)[colnames(data)=="WG_mean"] <- "u2"


  db<- data %>%
    mutate(Year=year(TimeStamp),Month=month(TimeStamp),Day=day(TimeStamp),Rs=(Rs*cnvrt),
           DOY=yday(TimeStamp),
           Kc = ifelse(DOY == DOY.ini.A, Kc.ini.A,
                       ifelse(DOY %in% DOY.ini.A:DOY.ini.B,(Kc.ini.B-Kc.ini.A)/(DOY.ini.B-DOY.ini.A)*(DOY-DOY.ini.A)+Kc.ini.A,
                              ifelse(DOY %in% DOY.ini.B:DOY.dev,(Kc.dev-Kc.ini.B)/(DOY.dev-DOY.ini.B)*(DOY-DOY.ini.B)+Kc.ini.B,
                                     ifelse(DOY %in% DOY.dev:DOY.mid, (Kc.mid-Kc.dev)/(DOY.mid-DOY.dev)*(DOY-DOY.dev)+Kc.dev,
                                            ifelse(DOY %in% DOY.mid:DOY.late, Kc.mid,
                                                   ifelse(DOY %in% DOY.late:DOY.harv,(Kc.late-Kc.mid)/(DOY.harv-DOY.late)*(DOY-DOY.late)+Kc.mid, 0)))))),
           Kc_corr=ifelse(DOY %in% DOY.mid:DOY.late,Kc+(0.04*(u2-2)-0.004*(RHmin-45))*(tree_height/3)^0.3 ,Kc))

  tryCatch({
    et_real_tot_in<-ReadInputs(varnames = c("RHmin","RHmax", "u2","Tmin","Tmax","Rs"),
                               climatedata = db,
                               stopmissing = c(30,30,30),
                               interp_missing_days = T,
                               interp_missing_entries = T,
                               interp_abnormal = T,
                               missing_method = "monthly average",
                               abnormal_method = "monthly average")
  }, error = function(e){et_real_tot_in<<-NULL})

  if(!is.null(et_real_tot_in)){

    if(!is.null(as.numeric(na.omit(unique(db$id)))) & is.null(Elev)){

      id=as.numeric(na.omit(unique(db$id)))
      Elev <- names_file[which(names_file$id==id),"alt"]


    }else{
      Elev=NULL
    }

    if(!is.null(as.numeric(na.omit(unique(db$id)))) & is.null(latitude)){

      id=as.numeric(na.omit(unique(db$id)))
      latitude <- names_file[which(names_file$id==id),"lat"]

    }else{
      latitude=NULL
    }

    if(is.null(Elev)){
      Elev <- 300
    }

    if(is.null(latitude)){
      latitude = 0.802851
    }else{
      latitude = as.numeric(latitude)*pi/180
    }

    constants=list(Elev=Elev,lambda=2.45,lat_rad=latitude,Gsc=0.0820,z=2,sigma=4.903*10^-9,G=0)


    et0_real<-ET_PenmanMonteith(data = et_real_tot_in,constants = constants,ts = "daily",
                                solar="data",wind = wind,crop = crop,message = message,save.csv = "no",
                                netRadiation=netRadiation)


    #df<-df %>% select(TimeStamp,id)
    df_ET<-bind_cols(db,ET0=as.numeric(et0_real$ET.Daily))

    df_ETc<-df_ET %>% mutate(
      ETc=ET0*Kc,
      ETc_corr=ET0*Kc_corr
    )

  }else {
    db <- db %>%
      mutate(doy=yday(TimeStamp))

    df_ETc<-left_join(db,etAvg) %>%
      mutate(ETc = ET,
             ETc_corr = ET,
             ET0 = ET/Kc) %>%
      select(-ET,-doy)
  }





  # if(any(is.na(df_ETc$ETc))){
  #   df_ETc<-left_join(df_ETc,etAvg,by=c("DOY"="doy")) %>%
  #     mutate(ETc=ifelse(is.na(ETc),ET,ETc)) %>%
  #     select(-ET)
  # }

  df_ETc<-df_ETc%>%
    select(-Year,-Month,-Day,-DOY)

  colnames(df_ETc)[colnames(df_ETc)=="Rs"] <- "GS_mean"
  colnames(df_ETc)[colnames(df_ETc)=="RHmin"] <- "LF_min"
  colnames(df_ETc)[colnames(df_ETc)=="RHmax"] <- "LF_max"
  colnames(df_ETc)[colnames(df_ETc)=="Tmax"] <- "LT_max"
  colnames(df_ETc)[colnames(df_ETc)=="Tmin"] <- "LT_min"
  colnames(df_ETc)[colnames(df_ETc)=="u2"] <- "WG_mean"

  if(!is.null(slope)){
    corr=1/cos(slope*pi/180)
    df_ETc$ETc = df_ETc$ETc*corr
    df_ETc$ETc_corr = df_ETc$ETc_corr*corr
  }

  df_ETc
}

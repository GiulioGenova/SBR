#' Get closest province of Bozen and SBR stations
#'
#' @export
#' @importFrom MonalisR getMeteoStat
#' @importFrom rgeos gDistance
#' @importFrom sp spTransform SpatialPoints CRS
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @import spdplyr
#'


# long=11.157978
# lat=46.557158
# sensor=c("LT","WG","LF","N","GS")
# load("SBR_names.RData")


getClosestStations<-function(long,lat,provSensor=NULL){


sp <- getMeteoStat(format = "spatial")
crsProv <- sp@proj4string


crsSbr<-names_file_sp@proj4string

point <- cbind(LONG=long,LAT=lat)
point <- SpatialPoints(point,proj4string = CRS("+init=epsg:4326"))

pointProv <- spTransform(point, CRS = crsProv)
pointSbr <- spTransform(point, CRS = crsProv)
names_file_sp <- spTransform(names_file_sp,CRS = crsProv)

if(is.null(provSensor)){
provSensor=get_provBz_sensors()$Sensor %>% unique
}


distSbr <- gDistance(spgeom1 = pointSbr,spgeom2 = names_file_sp,byid = T)

minDist<- min(distSbr)

minDistId <- which(distSbr==min(distSbr))

SbrClosest<-names_file_sp@data[minDistId,"id"]



provDistances <- lapply(provSensor,
                        function(provSensor,point,sp,crs){

  se <- get_provBz_sensors() %>% filter(Sensor == provSensor,!is.na(VALUE))
  sp <- left_join(sp,se)

  sp <- sp %>% filter(!is.na(Sensor))# for some reason filter with Sensro== sensor is not working


  dist <- gDistance(spgeom1 = point,spgeom2 = sp,byid = T)

  minDist<- min(dist)

  minDistId <- which(dist==min(dist))

  sp@data[minDistId,c("SCODE","Sensor")]
},

point=pointProv,sp = sp,crs = crsProv)

provClosest <- bind_rows(provDistances)

ret <- list(sbr=SbrClosest,prov=provClosest)

return(ret)

}

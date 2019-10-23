#' Get closest province of Bozen and SBR stations
#'
#' @export
#' @importFrom MonalisR getMeteoInfo
#' @importFrom sf st_crs st_transform st_sfc st_point st_as_sf st_distance st_drop_geometry
#' @importFrom dplyr bind_rows filter left_join distinct
#'


# long=11.157978
# lat=46.557158
# sensor=c("LT","WG","LF","N","GS")
# load("SBR_names.RData")


getClosestStations<-function(long,lat,provSensor=NULL){


  sp <- MonalisR::getMeteoInfo(format = "spatial") %>% st_transform(32632)# %>% dplyr::distinct(SCODE,.keep_all = T)
  #sp = as(sp,"Spatial")# from now on sp is used for spatial analysis. TO DO update code to use sf only.
  #crsProv <- sp@proj4string
  crsProv<-st_crs(sp)

  #crsSbr<-names_file_sp@proj4string

  point <- cbind(LONG=long,LAT=lat)
  #point <- SpatialPoints(point,proj4string = CRS("+init=epsg:4326"))
  point <- st_sfc(st_point(point),crs = 4326)
  pointProv <- st_transform(point,crsProv)#spTransform(point, CRS = crsProv)
  pointSbr <- pointProv#spTransform(point, CRS = crsProv)
  names_file_sp_sf <- #spTransform(names_file_sp,CRS = crsProv) %>%
    names_file_sp_sf %>% st_transform(crsProv) %>% #%>% st_as_sf()
    filter(id %in% c(3,
                     7,
                     9,
                     12,
                     14,
                     17,
                     #30,
                     37,
                     39,
                     52,
                     70,
                     84,
                     103,
                     105,
                     106,
                     125,
                     169,
                     171,
                     172,
                     174,
                     176))

  if(is.null(provSensor)){
    provSensor=get_provBz_sensors()$TYPE %>% unique
  }

  #distSbr <- gDistance(spgeom1 = pointSbr,spgeom2 = names_file_sp,byid = T)
  distSbr <- st_distance(x = pointSbr,y = names_file_sp_sf)
  minDist<- min(distSbr)

  minDistId <- which(distSbr==min(distSbr))

  #SbrClosest<-names_file_sp@data[minDistId,"id"]
  SbrClosest<-names_file_sp_sf[[minDistId,"id"]]


  provDistances <- lapply(provSensor,
                          function(provSensor,point,sp,crs){
                            #provSensor = "LT"
                            #se <- get_provBz_sensors() %>% filter(TYPE == provSensor,!is.na(VALUE)) %>% select(TYPE)
                            #sp <- left_join(sp,se)

                            #sp <- sp %>% filter(!is.na(TYPE))# for some reason filter with Sensro== sensor is not working
                            sp <- sp %>% filter(TYPE == provSensor,!is.na(VALUE))

                            #dist <- gDistance(spgeom1 = point,spgeom2 = sp,byid = T)
                            dist <- st_distance(x = point,y = sp)
                            minDist<- min(dist)

                            minDistId <- which(dist==min(dist))

                            #sp@data[minDistId,c("SCODE","TYPE")]
                            sp[minDistId,c("SCODE","TYPE")] %>% st_drop_geometry()
                          },

                          point=pointProv,sp = sp,crs = crsProv)


  provClosest <- bind_rows(provDistances) %>% distinct()
  ret <- list(sbr=SbrClosest,prov=provClosest)

  return(ret)

}

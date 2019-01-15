#' Computes Evapotraspiration using PenmanMonteith: modified from evapotranspiration package
#' in order to choose between net or global radiation
#'
#' @export
#' @import Evapotranspiration
#' @import zoo
#'



ET_PenmanMonteith<-function (data, constants, ts = "daily", solar = "sunshine hours",
                             wind = "yes", crop = "short", message = "yes", save.csv = "yes", netRadiation="no",
                             ...)
{
  if (is.null(data$Tmax) | is.null(data$Tmin)) {
    stop("Required data missing for 'Tmax' and 'Tmin', or 'Temp'")
  }
  if (is.null(data$va) | is.null(data$vs)) {
    if (is.null(data$RHmax) | is.null(data$RHmin)) {
      stop("Required data missing: need either 'va' and 'vs', or 'RHmax' and 'RHmin' (or 'RH')")
    }
  }
  if (wind == "yes") {
    if (is.null(data$u2) & is.null(data$uz)) {
      stop("Required data missing for 'uz' or 'u2'")
    }
  }
  if (solar == "data" & is.null(data$Rs)) {
    stop("Required data missing for 'Rs'")
  }
  else if (solar == "sunshine hours" & is.null(data$n)) {
    stop("Required data missing for 'n'")
  }
  else if (solar == "cloud" & is.null(data$Cd)) {
    stop("Required data missing for 'Cd'")
  }
  else if (solar == "monthly precipitation" & is.null(data$Precip)) {
    stop("Required data missing for 'Precip'")
  }
  if (wind != "yes" & wind != "no") {
    stop("Please choose if actual data will be used for wind speed from wind = 'yes' and wind = 'no'")
  }
  if (wind == "yes") {
    if (crop != "short" & crop != "tall") {
      stop("Please enter 'short' or 'tall' for the desired reference crop type")
    }
    else {
      alpha <- 0.23
      if (crop == "short") {
        z0 <- 0.02
      }
      else {
        z0 <- 0.1
      }
    }
  }
  else {
    z0 <- 0.02
    alpha <- 0.25
  }
  Ta <- (data$Tmax + data$Tmin)/2
  if (!is.null(data$va) & !is.null(data$vs)) {
    vabar <- data$va
    vas <- data$vs
  }
  else {
    vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax +
                                                 237.3))
    vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin +
                                                 237.3))
    vas <- (vs_Tmax + vs_Tmin)/2
    vabar <- (vs_Tmin * data$RHmax/100 + vs_Tmax * data$RHmin/100)/2
  }
  P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26
  delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta +
                                                                237.3)^2)
  gamma <- 0.00163 * P/constants$lambda
  d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)
  delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
  w_s <- acos(-tan(constants$lat_rad) * tan(delta2))
  N <- 24/pi * w_s
  R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) *
                                               sin(delta2) + cos(constants$lat_rad) * cos(delta2) *
                                               sin(w_s))
  R_so <- (0.75 + (2 * 10^-5) * constants$Elev) * R_a
  if (solar == "data") {
    R_s <- data$Rs
  }
  else if (solar != "monthly precipitation") {
    R_s <- (constants$as + constants$bs * (data$n/N)) *
      R_a
  }
  else {
    R_s <- (0.85 - 0.047 * data$Cd) * R_a
  }
  if (netRadiation=="no") {
    R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) *
      ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4)/2 *
      (1.35 * R_s/R_so - 0.35)
    R_nsg <- (1 - alpha) * R_s
    R_ng <- R_nsg - R_nl
  }else if (netRadiation=="yes") {
    R_ng <- R_s
  }


  if (wind == "yes") {
    if (is.null(data$u2)) {
      u2 <- data$uz * 4.87/log(67.8 * constants$z - 5.42)
    }
    else {
      u2 <- data$u2
    }
    if (crop == "short") {
      r_s <- 70
      CH <- 0.12
      ET_RC.Daily <- (0.408 * delta * (R_ng - constants$G) +
                        gamma * 900 * u2 * (vas - vabar)/(Ta + 273))/(delta +
                                                                        gamma * (1 + 0.34 * u2))
    }
    else {
      r_s <- 45
      CH <- 0.5
      ET_RC.Daily <- (0.408 * delta * (R_ng - constants$G) +
                        gamma * 1600 * u2 * (vas - vabar)/(Ta + 273))/(delta +
                                                                         gamma * (1 + 0.38 * u2))
    }
    ET.Daily <- ET_RC.Daily
    ET.Monthly <- aggregate(ET.Daily, as.yearmon(data$Date.daily,
                                                 "%m/%y"), FUN = sum)
    ET.Annual <- aggregate(ET.Daily, floor(as.numeric(as.yearmon(data$Date.daily,
                                                                 "%m/%y"))), FUN = sum)
  }
  else {
    RHmean <- (data$RHmax + data$RHmin)/2
    R_s.Monthly <- aggregate(R_s, as.yearmon(data$Date.daily,
                                             "%m/%y"), mean)
    R_a.Monthly <- aggregate(R_a, as.yearmon(data$Date.daily,
                                             "%m/%y"), mean)
    Ta.Monthly <- aggregate(Ta, as.yearmon(data$Date.daily,
                                           "%m/%y"), mean)
    RHmean.Monthly <- aggregate(RHmean, as.yearmon(data$Date.daily,
                                                   "%m/%y"), mean)
    ET_RC.Monthly <- 0.038 * R_s.Monthly * sqrt(Ta.Monthly +
                                                  9.5) - 2.4 * (R_s.Monthly/R_a.Monthly)^2 + 0.075 *
      (Ta.Monthly + 20) * (1 - RHmean.Monthly/100)
    ET_RC.Daily <- data$Tmax
    for (cont in 1:length(data$i)) {
      ET_RC.Daily[(((as.numeric(as.yearmon(time(ET_RC.Daily)))) -
                      floor(as.numeric(as.yearmon(time(ET_RC.Daily))))) *
                     12 + 1) == data$i[cont]] <- ET_RC.Monthly[cont]
    }
    ET.Daily <- ET_RC.Daily
    ET.Monthly <- aggregate(ET.Daily, as.yearmon(data$Date.daily,
                                                 "%m/%y"), FUN = sum)
    ET.Annual <- aggregate(ET.Daily, floor(as.numeric(as.yearmon(data$Date.monthly,
                                                                 "%m/%y"))), FUN = sum)
  }
  ET.MonthlyAve <- ET.AnnualAve <- NULL
  for (mon in min(as.POSIXlt(data$Date.daily)$mon):max(as.POSIXlt(data$Date.daily)$mon)) {
    i = mon - min(as.POSIXlt(data$Date.daily)$mon) + 1
    ET.MonthlyAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$mon ==
                                        mon])
  }
  for (year in min(as.POSIXlt(data$Date.daily)$year):max(as.POSIXlt(data$Date.daily)$year)) {
    i = year - min(as.POSIXlt(data$Date.daily)$year) + 1
    ET.AnnualAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$year ==
                                       year])
  }
  if (wind == "no") {
    ET_formulation <- "Penman-Monteith (without wind data)"
    ET_type <- "Reference Crop ET"
    Surface <- paste("short grass, albedo =", alpha, "; roughness height =",
                     z0, "m")
  }
  else {
    if (crop == "short") {
      ET_formulation <- "Penman-Monteith FAO56"
      ET_type <- "Reference Crop ET"
      Surface <- paste("FAO-56 hypothetical short grass, albedo =",
                       alpha, "; surface resistance =", r_s, "sm^-1; crop height =",
                       CH, " m; roughness height =", z0, "m")
    }
    else {
      ET_formulation <- "Penman-Monteith ASCE-EWRI Standardised"
      ET_type <- "Reference Crop ET"
      Surface <- paste("ASCE-EWRI hypothetical tall grass, albedo =",
                       alpha, "; surface resistance =", r_s, "sm^-1; crop height =",
                       CH, " m; roughness height =", z0, "m")
    }
  }
  if (solar == "data") {
    message1 <- "Solar radiation data have been used directly for calculating evapotranspiration"
  }
  else if (solar == "sunshine hours") {
    message1 <- "Sunshine hour data have been used for calculating incoming solar radiation"
  }
  else if (solar == "cloud") {
    message1 <- "Cloudiness data have been used for calculating sunshine hour and thus incoming solar radiation"
  }
  else {
    message1 <- "Monthly precipitation data have been used for calculating incoming solar radiation"
  }
  if (wind == "yes") {
    message2 <- "Wind data have been used for calculating the reference crop evapotranspiration"
  }
  else {
    message2 <- "Alternative calculation for reference crop evapotranspiration without wind data have been performed"
  }
  results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly,
                  ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve,
                  ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation,
                  ET_type = ET_type, message1 = message1, message2 = message2)
  if (ts == "daily") {
    res_ts <- ET.Daily
  }
  else if (ts == "monthly") {
    res_ts <- ET.Monthly
  }
  else if (ts == "annual") {
    res_ts <- ET.Annual
  }
  if (message == "yes") {
    message(ET_formulation, " ", ET_type)
    message("Evaporative surface: ", Surface)
    message(message1)
    message(message2)
    message("Timestep: ", ts)
    message("Units: mm")
    message("Time duration: ", time(res_ts[1]), " to ",
            time(res_ts[length(res_ts)]))
    if (NA %in% res_ts) {
      message(length(res_ts), " ET estimates obtained; ",
              length(which(is.na(res_ts))), " NA output entries due to missing data")
      message("Basic stats (NA excluded)")
      message("Mean: ", round(mean(res_ts, na.rm = T),
                              digits = 2))
      message("Max: ", round(max(res_ts, na.rm = T), digits = 2))
      message("Min: ", round(min(res_ts, na.rm = T), digits = 2))
    }
    else {
      message(length(res_ts), " ET estimates obtained")
      message("Basic stats")
      message("Mean: ", round(mean(res_ts), digits = 2))
      message("Max: ", round(max(res_ts), digits = 2))
      message("Min: ", round(min(res_ts), digits = 2))
    }
  }
  if (save.csv == "yes") {
    for (i in 1:length(results)) {
      namer <- names(results[i])
      write.table(as.character(namer), file = "ET_PenmanMonteith.csv",
                  dec = ".", quote = FALSE, col.names = FALSE,
                  row.names = F, append = TRUE, sep = ",")
      write.table(data.frame(get(namer, results)), file = "ET_PenmanMonteith.csv",
                  col.names = F, append = T, sep = ",")
    }
    invisible(results)
  }
  else {
    return(results)
  }
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WB.R
\name{WB}
\alias{WB}
\title{Computes computes water balace from evapotranspiration and precipitation data coming from ET function,
mergeOldAndForecast function and getForecast function}
\usage{
WB(data, taw = 50, lmitWarning = 0.8, p = 0.5, startwb = NULL,
  irrig = 0)
}
\arguments{
\item{data}{a table output of SBR::ET or SBR::mergeOldAndForecast or SBR::getForecast functions}

\item{taw}{the total available water. Default 50}

\item{lmitWarning}{fraction of the taw below which the app suggests to irrigate. Default 0.8}

\item{p}{fraction of the taw defining the raw (readly available water)}

\item{startwb}{is the initial water balance. Default value is NULL which yields taw*p}

\item{irrig}{vector with irrigation. Same lenght as rows of data or one number for the starting irrigation (first day of the time series)}
}
\description{
Computes computes water balace from evapotranspiration and precipitation data coming from ET function,
mergeOldAndForecast function and getForecast function
}

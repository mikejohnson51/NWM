% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getReservoir.R
\name{getReservoir}
\alias{getReservoir}
\title{Get NWM reservior output}
\usage{
getReservoir(AOI, filelist, param)
}
\arguments{
\item{AOI}{a AOI to subset data (generated with AOI::getAOI())}

\item{filelist}{a list of filepaths (generated with getFiles)}

\item{param}{a reservoir paramater}
}
\value{
a matrix of values where columns are files and rows are COMIDs
}
\description{
Download reservoir output from the HydroShare Thredds server for a list of input files.
Parallized version using \code{doParallel} and \code{foreach}.
}

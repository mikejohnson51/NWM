% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_trimChannel.R
\name{trimChannel}
\alias{trimChannel}
\title{Trim data to key COMIDs}
\usage{
trimChannel(idList = NULL, data = NULL, param = NULL, time = NULL,
  f = NULL)
}
\arguments{
\item{idList}{list of ides}

\item{data}{flow data to be trime}

\item{param}{string used to name time varying column}

\item{time}{a vector of time inputs}
}
\value{
a wide data.frame
}
\description{
Trim data to key COMIDs
}
\author{
Mike Johnson
}

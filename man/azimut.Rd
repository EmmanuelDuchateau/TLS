% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Basics.R
\name{azimut}
\alias{azimut}
\title{Calculate Azimut}
\usage{
azimut(x1 = x1, y1 = y1, xp, yp)
}
\arguments{
\item{x1}{X coordinate of the point}

\item{y1}{Y coordinate of the point}

\item{xp}{X coordinate of the point, can be a vector of X coordinates}

\item{yp}{Y coordinate of the point, can be a vector of Y coordinates}
}
\value{
returns an azimut values in degree  (or a vector of azimnuts)
}
\description{
This function calculate the azimut between two points
}
\examples{
x1 = 0 ; y1 = 0
xp = runif(5) ; yp = runif(5)
azimut (x1, y1, xp, yp)

}

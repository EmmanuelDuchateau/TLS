% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Basics.R
\name{distance}
\alias{distance}
\title{Calculate distance}
\usage{
distance(x1, y1, xp, yp)
}
\arguments{
\item{x1}{X coordinate of the point}

\item{y1}{Y coordinate of the point}

\item{xp}{X coordinate of the point, can be a vector of X coordinates}

\item{yp}{Y coordinate of the point, can be a vector of Y coordinates}
}
\value{
returns an distance values in degree  (or a vector of distances)
}
\description{
This function calculate the distance between two points
}
\examples{
x1 = 0 ; y1 = 0
xp = runif(5) ; yp = runif(5)
azimut (x1, y1, xp, yp)

}

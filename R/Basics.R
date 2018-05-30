
#' Calculate Azimut
#'
#' This function calculate the azimut between two points
#'
#' @param x1 X coordinate of the point
#' @param y1 Y coordinate of the point
#' @param xp X coordinate of the point, can be a vector of X coordinates
#' @param yp Y coordinate of the point, can be a vector of Y coordinates
#'
#' @return returns an azimut values in degree  (or a vector of azimnuts)
#' @export
#'
#' @examples
#' x1 = 0 ; y1 = 0
#' xp = runif(5) ; yp = runif(5)
#' azimut (x1, y1, xp, yp)
#'
azimut<-function(x1 = x1, y1=y1, xp,yp){
  #Copy from the function 'deg' of the package 'circular'
  deg<-function (x) {  (x * 180) /pi  }

  if(xp>=x1 && yp>y1)
  {degr<-deg(atan(abs(yp-y1)/abs(xp-x1)))}
  if(xp<x1 && yp>=y1)
  {degr<-180 - deg(atan(abs(yp-y1)/abs(xp-x1)))}
  if(xp<=x1 && yp<y1)
  {degr<-180 + deg(atan(abs(yp-y1)/abs(xp-x1)))}
  if(xp>x1 && yp<=y1)
  {degr<-360 - deg(atan(abs(yp-y1)/abs(xp-x1)))}
  if(xp==x1 && yp==y1)
  {degr<-NA}
  return(degr)
}




#' Calculate distance
#'
#' This function calculate the distance between two points
#'
#' @param x1 X coordinate of the point
#' @param y1 Y coordinate of the point
#' @param xp X coordinate of the point, can be a vector of X coordinates
#' @param yp Y coordinate of the point, can be a vector of Y coordinates
#'
#' @return returns an distance values in degree  (or a vector of distances)
#' @export
#'
#' @examples
#' x1 = 0 ; y1 = 0
#' xp = runif(5) ; yp = runif(5)
#' azimut (x1, y1, xp, yp)
#'
distance<-function(x1,y1,xp,yp){
  d1<-sqrt((x1-xp)^2+(y1-yp)^2)
  return(d1)
}


#' Check and install packages
#'
#' This function tests whether the packages in the x list are loaded. It tests if they are installed too. If not, they will be installed
#'
#' @param x x is a list of packages
#'
#' @export
#'
#' @importFrom utils install.packages
ValidLibrary<- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}


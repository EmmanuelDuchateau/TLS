#' Zsoil : recalculates Z values
#'
#' This function recalculates the Z value based on the distance to the ground.
#'
#' @param Mydata This dataset should contain at least 3 columns with X, Y, Z values.
#' The .las (.laz) format is better for faster processing but a data.frame (or data.table) is acceptable.
#' @param Hsoil Indicates the height of the points held above ground to create the DTM.
#' If the field is generally flat, the 4 m by default are sufficient, however for a fairly steep terrain,
#' it may be necessary to increase the value of Zsoil.
#'
#'
#' @return Returns a file in the las format containing the columns X, Y and Z as well as the new column Zr.
#' @export
#'
Zsoil<-function(Mydata, Hsoil=4){
  #conversion en las si data.frame
  #Mydata<-tot1
  if(is(Mydata,"data.table") | is(Mydata,"data.frame")) {las<-LAS(Mydata)}
   Mydata@data[, pulseID := 1:nrow(Mydata@data)] #cree une colonne avec des valeurs de pulse (pour LidarAerien)
    hsel<-min(Mydata@data$Z)+Hsoil
    las1<-lasfilter(Mydata, Z<hsel) %>% lasdecimate(100, homogenize = FALSE)

    ws = seq(2,10, 3)
    th = seq(0.1, 1, length.out = length(ws))
    lasground(las1, "pmf", ws, th)
     plot(las1, color = "Classification")
    # terrain = grid_terrain(las1, res=1, method = "delaunay")
     terrain = grid_terrain(las1, res=0.5, method = "kriging", k = 10)
    #terrain = grid_terrain(las1, method = "knnidw", k = 6)

    #plot(terrain1)
    lasnormalize = function(.las, dtm = NULL, method = "none", k = 10L, model = gstat::vgm(.59, "Sph", 874))
    {
      . <- Z <- Zn <- X <- Y <- Classification <- NULL

      # stopifnotlas(.las)

      if(is.null(dtm))
      {
        # normalized = LAS(data.table::copy(.las@data), .las@header)
        Zground = interpolate(.las@data[Classification == 2, .(X,Y,Z)], .las@data[, .(X,Y)], method = method, k = k, model = model)
        .las@data[, Zn := Zground][]
       }
      else
      {
        if(is(dtm, "lasmetrics"))
          dtm = as.raster(dtm)

        if(!is(dtm, "RasterLayer"))
          stop("The terrain model is not a RasterLayer or a lasmetrics", call. = F)

        #normalized = LAS(data.table::copy(.las@data), .las@header)
        lasclassify(.las, dtm, "Zn")
        #isna = is.na(normalized@data$Zn)
      }

      .las@data[, Zr := round(Z - Zn, 3)][, Zn := NULL][]
      #normalized = lasfilter(normalized, !isna)

      return(invisible())
    }
    #on normalise les Z par rapport au MNT terrain
    lasnormalize(Mydata, terrain)
    #Mydata@data$Zr = round(Mydata@data$Z - Mydata@data$Zn, 3)

    Mydata@data$pulseID<-NULL
    return(Mydata)
}


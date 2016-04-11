upslope.area.custom <- function (dem, log = TRUE, atb = FALSE, deg = 0.1, fill.sinks = TRUE) 
{
  if (!all.equal(xres(dem), yres(dem))) {
    stop("Raster has differing x and y cell resolutions. Check that it is in a projected coordinate system (e.g. UTM) and use raster::projectRaster to reproject to one if not. Otherwise consider using raster::resample")
  }
  
  if (fill.sinks) {
    capture.output(dem <- invisible(raster::setValues(dem, 
                                                      topmodel::sinkfill(raster::as.matrix(dem), res = xres(dem), 
                                                                         degree = deg))))
  }
  
  topidx <- topmodel::topidx(raster::as.matrix(dem), res = xres(dem))
  a <- raster::setValues(dem, topidx$area)
  if (log) {
    a <- log(a)
  }
  if (atb) {
    atb <- raster::setValues(dem, topidx$atb)
    a <- addLayer(a, atb)
    names(a) <- c("a", "atb")
  }
  return(a)
}
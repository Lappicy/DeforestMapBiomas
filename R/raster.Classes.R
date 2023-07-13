# Create a mask for the images selected according to a classification given ####
raster.Classes <- function(tif.files,
                           classes,
                           class.name,
                           output.name){

  # Libraries used ####
  require(raster)


  # Function itself ####
  for(i.raster.Classes in 1:length(tif.files)){

    # Fechar conexoes
    closeAllConnections()

    # What is the file name
    year.proxy <- as.numeric(substr(tif.files[i.raster.Classes],
                                    nchar(tif.files[i.raster.Classes]) - 7,
                                    nchar(tif.files[i.raster.Classes]) - 4))

    # Pull the tif image as a raster file
    tif.raster <- raster::raster(tif.files[i.raster.Classes])

    # Change raster to binary form
    binary.raster <- tif.raster[[names(tif.raster)]] %in% classes

    # Export this binary raster
    raster::writeRaster(x = binary.raster,
                        filename = paste(output.name, year.proxy, class.name,
                                         ".tif", sep = "_"))

    # close unwanted connections
    close.geo.connections()
  }


  # Return (NULL) ####
  return(NULL)
}

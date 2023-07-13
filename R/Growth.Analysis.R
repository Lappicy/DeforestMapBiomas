# Complete function (Growth.Analysis) ####
Growth.Analysis <-
  function(geo.file, tif.folder,
           mesh.size,
           output.folder, output.name,
           MAPBIOMAS = NULL, PRODES = NULL, ...){

    # Function itself ####
    # Read geo.file
    geo.file <- read.geo(file.name = geo.file)

    # Create mesh
    mesh.geo.file <- create.mesh(geo.file = geo.file, mesh.size = mesh.size)

    # Get the complete files names where the tif.files are located (tif.folder)
    if(class(tif.folder) == "character"){
      tif.files.names <- list.files(tif.folder, full.names = TRUE)
      tif.files.used <- list.files(tif.folder, full.names = TRUE)
    }
    if(class(tif.folder) %in% c("list", "RasterStack")){
      tif.files.names <- names(tif.folder)
      tif.files.used <- tif.folder
    }

    # Calculate the classes for each raster file
    for(i in 1:length(tif.files.names)){

      year.proxy <- substr(tif.files.names[i],
                           nchar(tif.files.names[i]) - 7,
                           nchar(tif.files.names[i]) - 4)

      raster.data.proxy <- calc.raster(geo.file = mesh.geo.file,
                                       tif.file = tif.files.used[[i]],
                                       year.used = year.proxy)


      if(i == 1) raster.data <- raster.data.proxy
      if(i > 1) raster.data <- base::merge(raster.data, raster.data.proxy,
                                           all = TRUE)

      remove(year.proxy, raster.data.proxy)
    }

    # Save this file (because it is time consuming) and remove folder
    write.table(x = raster.data,
                file = paste0(output.folder, "CalcRasterPixels.txt"),
                dec = ".", sep = "\t", quote = F, col.names = T, row.names = F,
                fileEncoding = "UTF-8")
    unlink("Proxy/", recursive = TRUE)


    # Transform pixel to km2
    raster.data.km2 <- pixel.to.km2(proxy.table = raster.data)

    # Count classes
    raster.data.km2.classes <- count.classes(proxy.table = raster.data.km2,
                                             MAPBIOMAS = MAPBIOMAS,
                                             PRODES = PRODES)

    # Calculate growth
    raster.data.km2.growth <- count.growth(proxy.table = raster.data.km2.classes,
                                           output.folder = output.folder,
                                           output.name = output.name)

    # Make the df to geopackage
    final.table <- base::merge(mesh.geo.file, raster.data.km2.growth)

    # Save as geopackage
    sf::st_write(final.table, append = FALSE,
                 paste0(output.folder,
                        substr(output.name, 1, nchar(output.name) - 4),
                        ".gpkg"))

    # Save as geopackage the simplified version
    sf::st_write(final.table[,!is.na(as.numeric(colnames(final.table)))],
                 append = FALSE,
                 paste0(output.folder, "simplified_",
                        substr(output.name, 1, nchar(output.name) - 4),
                        ".gpkg"))

    # Return ####
    return(final.table)
}

# Based on a geospatial file, sum all classification of a raster ####
calc.raster <- function(geo.file, tif.file, year.used = NULL,
                        folder.output = "Proxy/"){

  # Libraries used ####
  require(sf)
  require(raster)


  # Function itself ####
  # Open geo.file and tif
  geo.file <- read.geo(geo.file)
  tif.file <- raster::raster(tif.file)

  # Change the geo.file to only data.frame (lighter)
  geo.df <- sf::st_drop_geometry(geo.file)

  # Do the calculation for each row of this data.frame
  for(i.calc.raster in 1:nrow(geo.df)){

    # crop the tif.file and create a mask for it
    tif.crop <- raster::crop(x = tif.file, y = geo.file[i.calc.raster,])
    tif.mask <- raster::mask(x = tif.crop, mask = geo.file[i.calc.raster,])

    # Turn the raster into a matrix and get a table with summary data
    tif.matrix <- raster::as.matrix(tif.mask)
    tif.classes <- base::table(as.vector(tif.matrix))

    # Create a data.frame from this
    tif.df <- data.frame(as.list(tif.classes), check.names = FALSE)

    # Create proxy table to jon geospatial with raster
    if(nrow(tif.df) > 0) proxy.table <- base::merge(geo.df[i.calc.raster,],
                                                    tif.df, all = TRUE)
    if(nrow(tif.df) == 0) proxy.table <- geo.df[i.calc.raster,]

    # Create year column
    proxy.table$Year <- year.used

    # Save file to folder.output (if it is not NULL)
    if(!is.null(folder.output)){

      # if the number is less than 10, add three zeros
      # if the number is less than 100, add two zeros
      # if the number is less than 1000, add one zero
      name.calc.raster <-
        ifelse(i.calc.raster < 10, paste0("000", i.calc.raster),
               ifelse(i.calc.raster < 100, paste0("00", i.calc.raster),
                      ifelse(i.calc.raster < 1000, paste0("0", i.calc.raster),
                             i.calc.raster)))

      # If the output doesnt exist, create it
      dir.create(file.path(getwd(), folder.output), showWarnings = FALSE)

      # Save the file with the name
      write.table(x = proxy.table,
                  file = paste0(folder.output, year.used, "_",
                                name.calc.raster, ".txt"),
                  sep = "\t", dec = ".", row.names = FALSE, quote = FALSE,
                  fileEncoding = "UTF-8")
    }

    # Create final table (merge proxys)
    if(i.calc.raster == 1) final.table <- proxy.table
    if(i.calc.raster != 1) final.table <- base::merge(final.table, proxy.table,
                                                      all = TRUE)
  }

  # Reorder columns in an alfa numeric manner
  alfa.named.columns <- which(is.na(as.numeric(colnames(final.table))))
  numeric.named.columns <- which(!is.na(as.numeric(colnames(final.table))))
  numeric.named.columns <- sort(as.numeric(colnames(final.table)[numeric.named.columns]))
  numeric.named.columns <- match(as.character(numeric.named.columns), colnames(final.table))
  final.table <- final.table[,c(alfa.named.columns, numeric.named.columns)]


  # Return ####
  # Return the final table for all the lines from a geospatial file
  return(final.table)
}

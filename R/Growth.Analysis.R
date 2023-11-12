# Complete function (Growth.Analysis) ####
Growth.Analysis <-
  function(geo.file, tif.folder,
           mesh.size,
           output.folder, output.name,
           MAPBIOMAS = NULL, PRODES = NULL, ...){

    # Dependencies ####
    require(dplyr)
    require(raster)
    require(sf)


    # Function itself ####
    # Read geo.file
    geo.file <- read.geo(file.name = geo.file)

    # Create mesh
    mesh.geo.file <- create.mesh(geo.file = geo.file, mesh.size = mesh.size)

    # Get the complete files names where the tif.files are located (tif.folder)
    tif.files.names <- list.files(tif.folder, full.names = TRUE)

    # Calculate the classes for each raster file
    raster.data <-
      lapply(tif.files.names, FUN = function(x.int){

        # Separar oq tiver ponto
        year.proxy <- strsplit(as.character(x.int), split = ".", fixed = TRUE)

        # pegar o penúltimo elemento (antes do .tif ou .tiff)
        year.proxy <- sapply(year.proxy, "[[", length(year.proxy[[1]]) - 1)

        # Pegar os últimos 4 caracteres
        year.proxy <- substr(year.proxy, nchar(year.proxy) - 3, nchar(year.proxy))

        # Rodar o calc.raster usando esse ano específico como uma coluna
        raster.data.proxy <- calc.raster(geo.file = mesh.geo.file,
                                         tif.file = as.character(x.int),
                                         year.used = year.proxy)

        return(raster.data.proxy)
      })

    # Change the list to a single data.frame with all the columns
    raster.data <- dplyr::bind_rows(raster.data)
    raster.data <- raster.data[order(raster.data$ID_mesh),]

    # If the output doesnt exist, create it
    dir.create(file.path(getwd(), output.folder), showWarnings = FALSE)

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


    # Return ####
    return(raster.data.km2.growth)
  }

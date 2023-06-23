# Uaca 1 e 2 example ####
FinalAnalysis <-
  Growth.Analysis(geo.file = "Data/GPKG/Uaca_1_2.gpkg",
                  tif.folder = "Data/MapBiomas71/",
                  mesh.size = 0.045,
                  output.folder = "Results",
                  output.name = "Analysis_Uaca.txt",
                  MAPBIOMAS = 7.1)
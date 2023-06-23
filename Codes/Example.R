# Uaca 1 e 2 example ####
FinalAnalysis <-
  Growth.Analysis(geo.file = "Data/GPKG/Uaca_1_2.gpkg",
                  tif.folder = "Data/MapBiomas71/",
                  mesh.size = 0.045,
                  output.folder = "Results/",
                  output.name = "Analysis_Uaca.txt",
                  MAPBIOMAS = 7.1)

# Graph number 1 ####
gg.deforestation.cor(proxy.table = FinalAnalysis,
                     type.one = "Growth",
                     type.two = "Deforestation",
                     color.one = c("purple", "grey50", "#EA9999", "darkorange"), 
                     color.two = "darkgreen",
                     save.as = "Results/Deforestation vs Growth.png",
                     title.name = "Analysis for Uaça 1 e 2")

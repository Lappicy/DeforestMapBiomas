# Caverna do Maroaga (Presidente Figueiredo) example ####
FinalAnalysis <-
  Growth.Analysis(geo.file = "Example application/Data/GPKG/Caverna do Maroaga.gpkg",
                  tif.folder = "Example application/Data/MapBiomas8",
                  mesh.size = 0.045,
                  output.folder = "Results/",
                  output.name = "Analysis_CavernaMaroaga",
                  MAPBIOMAS = 8)


# Graph number 1 (time series + correlation) ####
gg.deforestation.cor(proxy.table = FinalAnalysis,
                     comparison.name = "Growth",
                     comparison.color = c("purple", "grey50", "#EA9999", "darkorange"),
                     save.as = "Results/Maroaga Deforestation vs Growth.png",
                     title.name = "Analysis for Caverna do Maroaga")


# Graph number 2 (mesh map) ####
mesh.map(mesh.data = FinalAnalysis,
         save.map.as = "Results/Map acumulated deforestation.png",
         map.height = 2300, map.width = 2100, map.units = "px")

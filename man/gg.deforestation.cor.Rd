\name{gg.deforestation.cor}
\alias{gg.deforestation.cor}
\title{Graph for deforestation with other classes and highest correlation.}
\usage{
gg.deforestation.cor(proxy.table,
                     type.one = "Growth", type.two = "Deforestation",
                     color.one = c("purple", "grey50", "#EA9999", "darkorange"),
                     color.two = "darkgreen",
                     save.as = "Results/Deforestation vs Growth.png",
                     title.name = "Analysis for Uaça 1 e 2",
                     different.groups = NULL)
}
\arguments{
  \item{proxy.table}{Table with the correct format, having columns with the names from \code{type.one} and \code{type.two}.}
  \item{type.one}{Which is the first class analysed, used to compare to deforestation usually.}
  \item{type.two}{The main class for comparisson, usually "Deforestation".}
  \item{color.one}{Color pallete to be used with the \code{type.one} data.}
  \item{color.two}{Color pallete to be used with the \code{type.two} data.}
  \item{save.as}{Where to save the image and format.}
  \item{title.name}{Name of title to appear on the plot.}
  \item{different.groups}{Which different groups should be used to separate the images. One may choose to make this analysis for specific subgroup from the table given at \code{proxy.table}}
}
\value{
This function creates a graph from a given table. It also saves the graph in the \code{save.as} folder and name.
}
\description{
The function takes a table as input, \code{proxy.table}, needing to have columns defined by \code{type.one} and \code{type.two} names. The \code{type.one} column names may be just the first initial leetter of the entire name (e.g. "Growth_" representing "Growth_urban", "Growth_mining"...). It also analyses the pearson correlation between the \code{type.one} and \code{type.two} columns, printing the highest correlation found between these two groups. It then creates a ggplot2 graph for the time series given, also writing the highest correlation found.
}
\examples{
# Load data
data("Uaca_1_2")
data("MapBiomas_71_Uaca")

# Run previous analysis needed (takes about ~10min)
FinalAnalysis <-
  Growth.Analysis(geo.file = Uaca_1_2,
                  tif.folder = MapBiomas_71_Uaca,
                  mesh.size = 0.045,
                  output.folder = "Results/",
                  output.name = "Analysis_Uaca.txt",
                  MAPBIOMAS = 7.1)

# Run this function code
gg.deforestation.cor(proxy.table = FinalAnalysis,
                     type.one = "Growth",
                     type.two = "Deforestation",
                     color.one = c("purple", "grey50", "#EA9999", "darkorange"),
                     color.two = "darkgreen",
                     save.as = "Results/Deforestation vs Growth.png",
                     title.name = "Analysis for Uaça 1 e 2")
}

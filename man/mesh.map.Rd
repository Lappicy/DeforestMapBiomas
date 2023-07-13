\name{mesh.map}
\alias{mesh.map}
\title{Map for the mesh results given by the \code{Growth.Analysis()} function}
\usage{
mesh.map(mesh.data, class = "Deforestation", year.used = "all",
         col.limits = c(0, 1, 2, 5),
         col.used = c("white", "#E5E200", "#FC780D", "red", "darkred"),
         save.map.as,
         map.height = 3000, map.width = 2000, map.units = "px")
}
\arguments{
  \item{mesh.data}{Table containing information about a given year and class. The desired format is the same as tue output from the \code{Growth.Analysis()} function.}
  \item{class}{Which class you want the map to be about (column name from the \code{mesh.data=} argument)}
  \item{year.used}{Which year you want to analyze. If \code{year.used = "all"} or more than one year, then the acumulated growth of each class is presented on the map.}
  \item{col.limits}{Which are the limits to group the class chosen.}
  \item{col.used}{Which colors should be used for the limits proposed in \code{col.limits=} argument.}
  \item{save.map.as}{Name to save this map as, with the format at the end (e.g. ".png" or ".pdf").}
  \item{map.height}{The desired heigth when saving this map.}
  \item{map.width}{The desired width when saving this map.}
  \item{map.units}{In what units are the \code{map.height=} and \code{map.width=} arguments. The default is "px", or pixels.}
}
\value{
This function creates a map from the output of the \code{Growth.Analysis()} function. It is a gridded map showing different degrees of different classes for a given year or the acumulated growth value for such class.
}
\description{
This function takes the output from the \code{Growth.Analysis()} function, selecting only the \code{class=} column to be analysed. The function then groups these values for the \code{year.used=} argument, grouping the growth rate when the \code{year.used=} is bigger than one. It then groups these classes values for the \code{col.limits=} defined and creates a ggplot2 object. This object is also saved given the \code{save.map.as=}.
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
mesh.map(mesh.data = FinalAnalysis,
         save.map.as = "Results/Map acumulated deforestation.png",
         map.height = 2300, map.width = 2100, map.units = "px")
}

\name{geo.tif.projection}
\alias{geo.tif.projection}
\title{Closes the connections made using the functinos of these packages.}
\usage{
geo.tif.projection(geo.file, tif.file)
}
\arguments{
  \item{geo.file}{The geospatial file used}
  \item{tif.file}{The image / raster used}
}
\value{
This function returns the \code{geo.file} given with the same projection as the \code{tif.file}.
}
\description{
This function reads the geospatial file with the existing \code{read.geo} function from this package. It then checks the projection of both the \code{geo.file} and the \code{tif.file}. If the projections are different, the function uses a \code{st_transform} from the \code{sf} package to transform the \code{geo.file} to the same projection as the \code{tif.file}, and return it.
}
\examples{
# Get example data
data(Uaca_1_2)
data(MapBiomas_71_Uaca)
MapBiomas.proxy <- MapBiomas_71_Uaca[[1]]

# Make sure the projection is the same
geo.tif.projection(geo.file = Uaca_1_2, tif.file = MapBiomas.proxy)
}

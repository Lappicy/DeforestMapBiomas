\name{create.mesh}
\alias{create.mesh}
\title{Creates a square mesh, with a given resolution / size, for a given geospatial file.}
\usage{
create.mesh(geo.file, mesh.size = 0.25, mesh.format = "square")
}
\arguments{
  \item{geo.file}{A geo spatial file (.gpkg, .shp, sf file...)}
  \item{mesh.size}{Refinement of the mesh size. Its is in the same unit as the "geo.file" (so may be in degrees, meters...).}
  \item{mesh.format}{The geometrical format of the mesh (for now only "square" is available)}
}
\value{
This function returns a squared mesh / grid with the same extent as the \code{geo.file} provided.
}
\description{
This function uses the \code{read.geo} to read the spatial file given if it is not in a format opened in R already. It then creates a square mesh with the sides having length as defined by \code{mesh.size}. A new column named \code{ID_mesh} is created for further data manipulation.
}
\examples{
data(Uaca_1_2)

Uaca_mesh <- create.mesh(geo.file = Uaca_1_2, mesh.size = 0.1)
}

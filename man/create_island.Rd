% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_island.R
\name{create_island}
\alias{create_island}
\title{Create a Tile for an Island}
\usage{
create_island(tile_map, position)
}
\arguments{
\item{tile_map}{An \code{sfc_POLYGON} object representing the layout of a
tile map.}

\item{position}{Either a numeric vector of length 2 giving the coordinates
for the centroid of the new tile, or a string equal to "upper left",
"lower left", "upper right", or "lower right" indicating in which corner
of the original tile map the new tile should be located.}
}
\value{
Returns an object of class \code{sfc_POLYGON} representing a single
  tile of the same shape and size as the tiles in the original tile map.
}
\description{
Create a tile for an island that can be added to an existing tile map
layout.
}
\details{
Creates a single tile of the same shape and size as the tiles in the given
tile map. This tile can be added to the layout of the given tile map to
represent an island or region that is not connected to the region
represented by the given tile map. The location of the new tile is
determined by the \code{position} argument. Setting the \code{position}
argument equal to "upper left", "lower left", "upper right", or "lower
right" will generate a tile which is located in the specified corner of the
given tile map. Setting the \code{position} argument to a numeric vector of
length 2 will generate a tile whose centroid is located at the coordinates
given in the vector.
}
\examples{
library(sf)
northeast <- governors[c(6,7,17,18,19,27,28,30,36,37,43),]
tile_map <- generate_map(northeast$geometry, square = FALSE)
tile_map <- append(tile_map, create_island(tile_map, "lower right"))

}

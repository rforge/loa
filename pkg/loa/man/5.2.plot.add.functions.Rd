\name{5.2.plot.add.functions}
\alias{5.2.plot.add.functions}
\alias{add.functions}
\alias{add.XYZGhosts}
\alias{add.Y2Axis}
\alias{add.XYPolygon}
\alias{add.LonLatMap}

\title{
  General plot add... functions
}
\description{
  General functions for adding extra layers to existing 
  plots.
}

\usage{

add.XYZGhosts(object = trellis.last.object(), 
              ..., unit = "native", 
              ghost.panel = panel.loaPlot) 

add.Y2Axis(object = trellis.last.object(), 
              ..., unit = "native", 
              rescale = NULL) 

add.XYPolygon(object = trellis.last.object(),
              x = NULL, y = NULL, data = NULL,
              ..., unit = "native",
              y2.scale=FALSE, first=FALSE)

add.LonLatMap(object = trellis.last.object(),
              ..., map = NULL, recolor.map=FALSE,
              show.axes = FALSE, unit = "native",
              first = TRUE)
                        
}


\arguments{

  \item{object}{ The plot to add an extra plot layer or 
  layers to. This defaults to last lattice plot produced 
  if not set in call. 
  } 
  
  \item{...}{ Additional arguments, passed on to related 
  functions.
  } 

  \item{unit}{ The plot units, by default native.
  } 
  \item{ghost.panel}{ The plot panel to use when adding 
  ghost points (lines, etc). 
  }

  \item{rescale}{ For \code{add.Y2Axis}, y data-series 
  rescaling, by default NULL/none.  
  } 

  \item{x,y}{ x and y data-series to be used.  
  } 
  
  \item{data}{ If supplied, the expected source of x 
  and y data-series.  
  } 
  
  \item{y2.scale}{ When building the Plot, use the second 
  y axis if that exists, default FALSE.  
  }

  \item{first}{ When building the plot add the new plot 
  layer first, default varies. 
  }
  
  \item{map,recolor.map,show.axes}{ Options when adding 
  a map layer, see loaMapPlot documentations.
  }
}

\details{
  \code{add.loaGhosts} adds ghost points showing the 
  locations of all points in all panels.
  
  \code{add.Y2Axis} adds a second y axis. 
  
  \code{add.XYPolygon} adds a supplied (x,y) polygon.
  
  \code{add.LonLatMap} adds a map layer.
}

\value{
  \code{add...} functions returns the supplied (or last) 
  lattice plot with an additional plot layer added.
}

\note{
  \code{add.XYZGhosts} and \code{add.LonLatMap} are 
  in-development and may change significantly in 
  future package versions.
}


\references{

  These functions make extensive use of code developed 
  by others.

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

  Grid (part of base R):
  R Core Team (2017). R: A language and environment 
  for statistical computing. R Foundation for 
  Statistical Computing, Vienna, Austria.
  URL https://www.R-project.org/.

}

\author{
  Karl Ropkins
}

%\seealso{
%   In other packages: See \code{\link{grid.locator}}; 
%   \code{\link{trellis.focus}} and \code{\link{lpoints}} in \code{\link{lattice}}.   
%   See \code{\link{LatLon2XY}} in \code{RgoogleMap}.  
%}

%\examples{
%##
%##to be confirmed
%##getXY on xyplot
%##example 2
%##Move getLatLon code to examples
%}

\keyword{ methods }

\name{4.9.loa.shapes}
\alias{4.9.loa.shapes}
\alias{loa.shapes}
\alias{loa.glyphs}
\alias{loaPolygon}
\alias{loaCircle}
\alias{loaPieSegment}

\title{
  loa shapes
}

\description{
  Simple shapes.
}
\usage{

loaPolygon(x, y, ..., polygon = NULL, 
            loa.scale = NULL)

loaCircle(..., polygon = NULL, radius = 1)

loaPieSegment(..., polygon = NULL, start = 0, 
            angle=360, radius = 1, center=TRUE)

}

\arguments{

  \item{x, y}{ The \code{x} and \code{y} points at which to plot the 
  requested shape.
  } 

  \item{...}{ Any additional arguments, usually passed on.
  } 

  \item{polygon}{ A list with elements \code{x} and \code{y} giving the 
   polygon/shape to be plotted.
  } 

  \item{loa.scale}{ A list of parameters that can be used to fine-tune the 
   \code{polygon} plotting.  
  } 

  \item{radius}{ The radius to used when drawing either circles or pie segments.
  }

  \item{start, angle}{ When drawing pie segments, \code{angle} the angle of 
  of the segment and \code{start} point.
  }

  \item{center}{ Should the segement begin and end at the center?  
  } 

}


\details{

  \code{loaPolygon} is a general function for drawing polygons. It is 
  intended as an alternative to \code{lpolygon}, and other standard 
  \code{loa...} shapes are typically wrappers for this function.

  \code{loaCircle} draws a circle with an origin at (\code{x}, \code{y}).

  \code{loaPieSegment} draws a pie segment (or slice of cake) shape. It 
  is typically used as building block for pie plots and other similar 
  glyph structures.

}

\value{
  All these functions generate simple shapes and are intended to be run 
  within \code{panel...} functions as building blocks for more complex 
  glyph type structures.

}
\references{
  These functions make extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5
}

\author{
  Karl Ropkins
}

\note{
  TO DO

}

\seealso{
   \code{\link{lattice}}, \code{\link{xyplot}}, 
}

\examples{

##example 1
##general

#TO DO

}


\keyword{ methods }

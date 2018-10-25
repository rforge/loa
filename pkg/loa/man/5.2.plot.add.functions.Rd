\name{5.2.plot.add.functions}
\alias{5.2.plot.add.functions}
\alias{add.functions}
\alias{add_loaPanel}

\title{
  General plot add... functions
}
\description{
  General functions for adding extra layers to existing plots.
}

\usage{

add_loaPanel(lattice.plot=trellis.last.object(),
             preprocess = NULL, panel = NULL, 
             postprocess = NULL, ...)
}

\arguments{

  \item{lattice.plot}{ The plot to add an extra layer or layers 
   to. This defaults to last lattice plot produced if not set in 
   call. 
  } 

  \item{preprocess}{ If set, a function to apply to 
  \code{lattice.plot} before adding new panel.
  } 

  \item{panel}{ If set, a new \code{panel...} function to add to  
  \code{lattice.plot} after the current \code{lattice.plot} panel. 
  } 

  \item{postprocess}{ If set, a function to apply to 
  \code{lattice.plot} after adding new panel.
  } 

  \item{...}{ Additional arguments, passed on to related functions.
  } 

}


\details{
  \code{add_loaPanel} is a frame work function for adding 
  additional layers to an exiting lattice plot.      
}

\value{
  \code{add_loaPanel} returns the supply (or last) lattice plot 
   with an additional plot layer added.
}

\note{
  \code{add_loaPanel} is in-development aand may change 
  significantly in future pacakge versions

}


\references{

  This function makes extensive use of code developed by others.

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

  Grid (part of base R):
  R Core Team (2017). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria.
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

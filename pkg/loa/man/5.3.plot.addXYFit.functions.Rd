\name{5.3.plot.add.XYFit.functions}
\alias{5.3.plot.add.XYFit.functions}
\alias{add.XYFit.functions}
\alias{add.XYLMFit}
\alias{add.XYLOESSFit} 
\alias{add.XYFit_prep}
\alias{loaXYFit_lm}
\alias{loaXYFit_loess}
\alias{panel.loaXYFit}
\alias{add.loaPanel}

\title{
  Plot add.XY...Fit functions
}
\description{
  Functions for adding XY fit model layers to existing plots.
}

\usage{

#main functions

add.loaPanel(lattice.plot = trellis.last.object(), 
           preprocess = NULL, panel = NULL, 
           postprocess = NULL, ..., 
           use = NULL, ignore = NULL) 

add.XYLMFit(lattice.plot = trellis.last.object(),
           preprocess = add.XYFit_prep,
           panel = panel.loaXYFit, ...)

add.XYLOESSFit(lattice.plot = trellis.last.object(),
           preprocess = add.XYFit_prep, 
           model.method = loaXYFit_loess,
           panel = panel.loaXYFit, ...)

#others

add.XYFit_prep(lattice.plot = trellis.last.object(),
           model.method = loaXYFit_lm, ...)

loaXYFit_lm(x, y, output.len = 25, formula.signif = 2, 
           r2.digits = 3, group.id = NULL, ...)

loaXYFit_loess(x, y, output.len = 25, r2.digits = 3, 
           group.id = NULL, ...)

panel.loaXYFit(...)

}

\arguments{

  \item{lattice.plot}{ The plot to add an extra layer or 
  layers to. This defaults to last lattice plot produced 
  if not set in call. 
  } 

  \item{preprocess}{ If set, a function to apply to 
  \code{lattice.plot} before adding new panel.
  } 

  \item{panel}{ If set, a new \code{panel...} function to 
  add to \code{lattice.plot} after the current 
  \code{lattice.plot} panel. 
  } 
  
  \item{postprocess}{ If set, a function to apply to 
  \code{lattice.plot} after the new panel.
  } 


  \item{...}{ Additional arguments, passed on to 
  daughter functions.
  } 

  \item{model.method}{ Function to use on XY data 
  to generate fit reports.
  } 
  
  \item{use,ignore}{ if set, the names of arguments 
  supplied to the previous plot that should be used 
  or ignored, respectively, by panel being added.
  }

  \item{x, y}{ XY data from plot, typically filtered 
  by both panel and group.
  }

  \item{output.len}{ Number of XY pairs to use when 
  making fit line standard error bands.  
  }

  \item{formula.signif, r2.digits}{ When number of 
  significant numbers and digits to use when reporting 
  the fit formula and r.squared, respectively.
  }

  \item{group.id}{ When XY data is grouped, the group 
  identity.
  }
}


\details{
  \code{add.loaPanel} adds a plot panel to an existing 
  loaPlot output. 
  
  \code{add.XYLMFit} and \code{add.XYLFit} add linear 
  and loess model fit surfaces to exiting lattice plot.  

  Both functions are intended to be used in form:

  [make plot, e.g. loaPlot(...) then] addXYLMFit(...)

  \code{loaXYFit_lm} and \code{loaXYFit_lm} generate fit and 
  report information for linear and loess fit surfaces. 

  Other functions contain code structure common to all 
  \code{addXYFit} functions.     

}

\value{
  \code{addXYLMFit} returns the supply (or last) lattice plot 
   with an added linear fit surface layer, generated using 
   \code{lm()}.

  \code{add.XYLMFit} returns the supply (or last) lattice plot 
   with an added linear fit surface layer, generated using 
   \code{loess()}.

  The elements of the XYFit layers are \code{fit} the fit line, 
  \code{se} the standard error bands, and \code{report} the 
  fit report panels. These track properties, e.g. \code{col}, etc, 
  of the plot they add to unless these terms are reset in the 
  addXYFit call. They can also be removed by setting to FALSE, 
  e.g. \code{fit=FALSE}, or updated individually using e.g. 
  \code{fit.col=2} to change just the fit line color to red.  

  The report position within the plot panel can be set using   
  report.position which uses a relative (x=0-1, y=0-1) scale 
  to position the report.
}

\note{
  These functions are in-development aand may change 
  significantly in future pacakge versions.
}


\references{

  This function makes extensive use of code developed by others.

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

  lm() and loess() in stats (part of base R):
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

\examples{

# add lm fit a loaPlot
addXYLMFit(loaPlot(cadmium~copper*zinc, data=lat.lon.meuse))

\dontrun{

loaPlot(cadmium~copper*zinc, data=lat.lon.meuse)
addXYLOESSFit(col="red")

p <- loaPlot(cadmium~copper*zinc, data=lat.lon.meuse)
addXYLMFit(p, se.col="grey", report.position=c(0.95, 0.22))

}

}

\keyword{ methods }

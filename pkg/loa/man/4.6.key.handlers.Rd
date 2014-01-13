\name{4.6.key.handlers}
\alias{4.6.key.handlers}
\alias{keyHandler}
\alias{draw.loaPlotZKey}
\alias{draw.loaColorKey}
\alias{draw.loaColorRegionsKey}
\alias{draw.zcasePlotKey}

\title{
  Key handling
}

\description{
  Workhorse functions for routine use of keys in plots.
}
\usage{

keyHandler(key = NULL, ..., output = "key")

#keys

draw.loaPlotZKey(key = NULL, draw = FALSE, vp = NULL, ...)

draw.loaColorKey(key = NULL, draw = FALSE, vp = NULL, ...)

draw.loaColorRegionsKey(key = NULL, draw = FALSE, vp = NULL, ...)

draw.zcasePlotKey(key = NULL, draw = FALSE, vp = NULL, ...)

}

\arguments{

  \item{key}{ The key to be used.
  } 

  \item{...}{ Any additional arguments to be used to modify the 
   the \code{key} before plotting.
  } 

  \item{output}{ The format to return the function output in. This is 
  \code{'key'} for all routine (in plot) use.  
  } 

  \item{draw, vp}{ \code{lattice} and \code{grid} arguments using when 
  plotting \code{GROB} objects. Generally, these can be ignored.  
  }

}


\details{
  \code{keyHandler} is a general function that rotuine generates 
  defaults arguments for add a key to a plot.  

  \code{draw...key} functions are all specialist plots keys. They are 
  typically modifications of or variations on similar functions in 
  \code{lattice}, e.g. \code{draw.key} and \code{draw.colorkey}.

  \code{draw.loaPlotZKey} is the default 'bubble plot' key used with 
  \code{\link{loaPlot}}.

  \code{draw.loaColorKey} and \code{draw.loaColorRegionsKey} are variations 
  on the \code{draw.colorkey} function in \code{\link{lattice}}.

  \code{draw.zcasePlotKey} is a simple legend based on \code{zcase} 
  annotation.
     
}

\value{
  \code{keyHandler} return a list of plot arguments to be used to generate 
  a key .

  When run within plot calls, the \code{draw...key} functions associated 
  color keys. If they are used with \code{loa} plots and suitable 
  \code{panel...} functions, color scales are automatically aligned. 
}

\references{
  These functions make extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

%  [ref section on adding colorscale to xyplot]

%  [ref for RColorBrewer to add]

%  [ref for grid, draw and vp]

}

\author{
  Karl Ropkins
}

\note{
  In Development: Function structures may change in future 
  package updates.
}

\seealso{
   In other packages: See \code{\link{xyplot}} in \code{\link{lattice}}. 
}

%\examples{
%##example 1
%##general
%#TO DO
%}


\keyword{ methods }

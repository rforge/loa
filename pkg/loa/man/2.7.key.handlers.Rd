\name{2.7.key.handlers}
\alias{2.7.key.handlers}
\alias{keyHandler}
\alias{draw.loaPlotZKey}
\alias{draw.loaColorKey}

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

}

\arguments{

  \item{key}{ The key to be used.
  } 

  \item{...}{ Any additional arguments to be used to modify the 
   the \code{key} before plotting.
  } 

  \item{output}{ The format to return the function output in. This is 
  \code{'key'} for all rotune (in plot) use. [FURTHER DOCUMENTATION 
   NEEDED]  
  } 

  \item{draw, vp}{ \code{lattice} and \code{grid} arguments using when 
  plotting \code{GROB} objects. Generally, these can be ignored. [LINK 
  FOR THOSE WANTING MORE INFORMATION?] 
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
     
}

\value{
  \code{keyHandler} return a list of plot arguments [DETAILS].

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

  [ref section on adding colorscale to xyplot]

  [ref for RColorBrewer to add]

  [ref for grid, draw and vp]

}

\author{
  Karl Ropkins
}

\note{
  IN DEVELOPMENT: Function structures may change in future 
  package updates.
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

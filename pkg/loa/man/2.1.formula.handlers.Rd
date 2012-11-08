\name{2.1.formula.handlers}
\alias{2.1.formula.handlers}
\alias{formulaHandler}


\title{
  Formula handling for lattice plots
}
\description{
  Function(s) for the formula element of lattice functions.
}
\usage{

  formulaHandler(x, data = NULL, ..., 
        check.xy.dimensions = TRUE)

}

\arguments{

  \item{x}{ A formula to used to generate a lattice plot. See Below.
   }

  \item{data}{ If supplied, the assumed source of the elements of formula 
   \code{x}, typically a \code{data.frame}.
   }

  \item{...}{ Additional arguments are passed on to related functions. 
   }

   \item{check.xy.dimensions}{For \code{formulaHandler} only, logical. Should the formula 
   structure be tested before attempting to generate the plot?  
   }

}

\details{
  NOTE: \code{formulaHandler} is currently in development and may be subject to changes.

  \code{formulaHandler} manages the formula component or \code{x} element of 
   of loa plot functions. 

   For example, for \code{loaPlot} it assumes the general formula structure 
   \code{z ~ x * y | cond}, and applies it in a similar fashion to the 
   \code{\link{lattice}} function \code{\link{levelplot}}. \code{X} and 
   \code{y} are the horizontal and vertical axes, \code{z} is any 
   additional information to be used in point, symbol, surface or glyph 
   generation, and \code{cond} any additional conditioning to be applied. 
   (\code{X} and \code{y} are required elements; \code{z} and \code{cond} 
   are typically optional.)
 
}

\value{

  \code{formulaHandler} returns a list, containing the plot elements 
   defined in the supplied formula.
   
}

\note{

  This function is in development and may be subject to changes.
     
}


\references{
  This function makes extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

  RColorBrewer:
  Erich Neuwirth <erich.neuwirth@univie.ac.at> (2011). RColorBrewer:
  ColorBrewer palettes. R package version 1.0-5.
  http://CRAN.R-project.org/package=RColorBrewer

}

\author{
  Karl Ropkins
}

\seealso{

   In loa: \code{\link{loaPlot}}; \code{\link{panelPal2}}

   For information of related functions in other packages, see

   \code{\link{lattice}}: \code{\link{levelplot}}.

}

\examples{
#TO BE CONFIRMED


}

\keyword{ methods }

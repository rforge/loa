\name{4.2.plot.structure.handlers}
\alias{4.2.plot.structure.handlers}
\alias{formulaHandler}
\alias{stripHandler}


\title{
  Handler functions for plot structure arguments.
}
\description{
  Function(s) for handling (front end) plot arguments like x and strip that manage 
  the plot structure.
}
\usage{

  formulaHandler(x, data = NULL, ..., 
        formula.type = "z~x*y|cond", 
        panel.zcases = FALSE,
        check.xy.dimensions = TRUE, 
        output = "extra.args")

  stripHandler(..., striplab=NULL)

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

   \item{formula.type}{For \code{formulaHandler} only, character vector. The plot 
   structure to be used when generating the plot, e.g. \code{z ~ x * y | cond} for 
   \code{\link{loaPlot}}  
   }

   \item{panel.zcases}{For \code{formulaHandler} only, logical. Should zcase arguments, 
   e.g. \code{z1} and \code{z2} in \code{z1 + z2 ~ x * y | cond}, be treated as 
   panel conditioning terms rather than grouping terms?  
   }

   \item{striplab}{For \code{stripHandler} only, character vector. If supplied, the 
   label to add to the panel strip when conditioning is applied. By, default if applies 
   the standard \code{lattice} convention, i.e., show for numerics.   
   }

   \item{output}{For \code{formulaHandler} only, character vector. The format to return 
   function output in.}

}

\details{
  \code{formulaHandler} manages the formula component or \code{x} element of 
   of loa plot functions. 

   For example, for \code{loaPlot} it assumes the general formula structure 
   \code{z ~ x * y | cond}, and applies it in a similar fashion to the 
   \code{\link{lattice}} function \code{\link{levelplot}}. \code{x} and 
   \code{y} are the horizontal and vertical axes, \code{z} is any 
   additional information to be used in point, symbol, surface or glyph 
   generation, and \code{cond} any additional conditioning to be applied. 
   (\code{X} and \code{y} are required elements; \code{z} and \code{cond} 
   are typically optional.)

  \code{stripHandler} manages the strip component of loa plot functions. 
 
}

\value{

  \code{formulaHandler} returns a list, containing the plot elements 
   defined in the supplied formula.

  \code{stripHandler} returns a list containing all supplied arguments, 
   subject to the following modifications:
   \tabular{ll}{
   * \tab strip generated or updated\cr
   * \tab (if supplied) striplab added as var.name\cr
   }
    
}

\note{

  These function are in development and may be subject to changes.
     
}


\references{
  This function makes extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

}

\author{
  Karl Ropkins
}

\seealso{

   In loa: \code{\link{loaPlot}}; \code{\link{panelPal}}

   For information of related functions in other packages, see

   \code{\link{lattice}}: \code{\link{levelplot}}.

}

\examples{
#TO BE CONFIRMED


}

\keyword{ methods }

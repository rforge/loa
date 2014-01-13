\name{4.2.plot.structure.handlers}
\alias{4.2.plot.structure.handlers}
\alias{formulaHandler}
\alias{stripHandler}
\alias{getZcaseDimensions}


\title{
  Handler functions for plot structure arguments.
}
\description{
  Function(s) for handling (front end) plot arguments like x and strip that manage 
  the plot structure.
}
\usage{

  formulaHandler(x, data = NULL, groups = NULL, 
        ..., expand.plot.args = TRUE, formula.type = "z~x*y|cond", 
        panel.zcases = FALSE, check.xy.dimensions = TRUE, 
        check.coord.dimensions = TRUE, get.zcase.dimensions = TRUE, 
        output = "extra.args")

  stripHandler(..., striplab=NULL)

  getZcaseDimensions(...)

}

\arguments{

  \item{x}{ A formula to used to generate a lattice plot. See Below.
   }

  \item{data}{ If supplied, the assumed source of the elements of formula 
   \code{x}, typically a \code{data.frame}.
   }

  \item{groups}{ If supplied, the grouping argument to be used with 
   \code{x} and \code{data}.
   }

  \item{...}{ Additional arguments are passed on to related functions. 
   }

   \item{expand.plot.args}{For \code{formulaHandler} only, logical. Should any 
   short elements of the plot structure be expanded? 
   }

   \item{formula.type}{For \code{formulaHandler} only, character vector or function. 
   The plot structure to be used when generating the plot, e.g. \code{z ~ x * y | cond} for 
   \code{\link{loaPlot}}  
   }

   \item{panel.zcases}{For \code{formulaHandler} only, logical. Should zcase arguments, 
   e.g. \code{z1} and \code{z2} in \code{z1 + z2 ~ x * y | cond}, be treated as 
   panel conditioning terms rather than grouping terms?  
   }

   \item{check.xy.dimensions, check.coord.dimensions}{For \code{formulaHandler} only, logicals. 
   Should the formula structure be tested before attempting to generate the plot? See Note below.  
   }

   \item{get.zcase.dimensions}{For \code{formulaHandler} only, logical. Should the dimensions 
   of any multiple \code{zcases} b calculated? See Note below.  
   }

   \item{output}{For \code{formulaHandler} only, character vector. The format to return 
   function output in.
   }

   \item{striplab}{For \code{stripHandler} only, character vector. If supplied, the 
   label to add to the panel strip when conditioning is applied. By default, it applies 
   the standard \code{lattice} convention, i.e., show for numerics.   
   }

}

\details{

  \code{formulaHandler} manages the formula component or \code{x} element of 
   of loa plot functions. 

   For example, for \code{loaPlot} it assumes the general formula structure 
   \code{z ~ x * y | cond}, and applies it in a similar fashion to the 
   \code{\link{lattice}} function \code{\link{levelplot}}. 

   Within the formula part of the plot call \code{x} and \code{y} are the 
   horizontal and vertical axes, \code{z} is any additional information to be 
   used in point, symbol, surface or glyph generation, and \code{cond} any 
   additional conditioning to be applied. (The coordinates, \code{x} and 
   \code{y}, are required elements; \code{z} and \code{cond} are typically 
   optional.) 

   \code{stripHandler} manages the strip component of loa plot functions.

   \code{getZcaseDimensions} tracks the dimensions of multiple \code{z} 
    
 
}

\value{

  \code{formulaHandler} returns a list, containing the plot elements 
   defined in the supplied formula.

  \code{stripHandler} returns a list containing all supplied arguments, 
   subject to the following modifications: \code{strip}, Updated or generated 
   if not supplied; \code{striplab}, added to \code{strip} via the \code{strip} 
   argument \code{var.name}, if this is undeclared in call.

  \code{getZcaseDimensions} returns a list containing all the supplied 
  arguments, plus two additions arguments (if supplied in the call): 
  \code{zcase.zlim} and \code{z.rowsum.lim}. \code{zcase.zlim} is a 
  list of \code{lim} values, one for each \code{zcase}. 
  \code{z.rowsum.lim} is the range of 'by-row' sums of \code{zcases}.  
  These are calculated using any \code{zcase} information supplied in 
  the call.  
    
}

\note{

  These function are in development and may be subject to changes.

  The current version of \code{formulaHandler} includes code from the 
  \code{stats} function \code{get_all_vars}. It operates in a similar 
  fashion to the previous version but checks zcase dimensions.

  The previous version of \code{formulaHandler} was a wrapper for 
  the \code{lattice} function \code{latticeParseFormula}. This 
  version of \code{formulaHandler} was updated to simplify multiple 
  \code{z} argument handling.

  The latest version of \code{formulaHandler} includes more flexible 
  \code{formula.type} handling. For example, it can now handle formulas 
  that have more than two coordinates. As a result the 
  \code{check.xy.dimensions} argument was replaced with a 
  \code{check.coord.dimensions} argument. The previous argument 
  will however remain in the function formals and function as before 
  until all related code has been updated.

  The latest version of \code{formulaHandler} uses \code{getZcaseDimensions} 
  to calculate the dimensions of \code{z} if it is multi-part, e.g. 
  \code{z1 + z2 + etc ~ x * y} rather than \code{z ~ x * y}.    
     
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

   In other packages: \code{\link{levelplot}} in \code{\link{lattice}}.

}

%examples to do
%\examples{
%#TO BE CONFIRMED
%}

\keyword{ methods }

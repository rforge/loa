\name{1.4.stackPlot}
\alias{1.4.stack.plots}
\alias{stackPlot}
\alias{panel.stackPlot}


\title{
  Stack plots
}

\description{
  Stack plot functions for Lattice. 
}

\usage{

   
   stackPlot(x, data = NULL, ...)

   #standard panels

   panel.stackPlot(..., process=TRUE, plot=TRUE, 
            loa.settings = FALSE) 

   #data handlers
   ##currently not exported
}

\arguments{

  \item{x}{ For \code{stackPlot} only, a formula with the general structure 
   \code{y ~ x} or \code{y1 + y2 ~ x | cond}, etc. The elements 
   \code{y1}, \code{y2}, etc are stacked on the y-axis, and plotted against x. 
   Both are required.   
   }

  \item{data}{ For \code{stackPlot} only, if supplied, the assumed source 
   of the elements of formula \code{x}, typically a \code{data.frame}.
   }

  \item{...}{ Additional arguments. 
   }

  \item{loa.settings, plot, process}{\code{\link{loaPlot}} arguments used to manage 
   \code{\link{panelPal}} activity.
   }  
  
}


\details{

   \code{stackPlot} generates a stack plot using the lattice framework.

   \code{panel.stackPlot} handles the appearance of triangle plot
    outputs. 

} 


\value{

   \code{stackPlot} returns trellis objects, much like 
   conventional \code{lattice} plot functions.

   \code{panel.stackPlot} is intended for use 
   within a \code{trianglePlot} function call. 

}

\references{

  These function makes extensive use of code developed by others.  

  As elsewhere, the use of \code{lattice} is also gratefully acknowledged:

  lattice:
  Sarkar, Deepayan (2008). Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5
}

\author{
  Karl Ropkins
}

\note{

   Development:

   This is an in-development plot, and 'best handling' strategies have not 
   been decided for several elements. So, future versions of these functions 
   may differ significantly from the current version. 

   In particular:

   \code{stackPlot}:

   The \code{stackPlot} argument \code{x} may include conditioning in the form 
   \code{y ~ x | cond}. However, exact handling is has not been defined, so may 
   subject to change.   

   To generate the stacks, \code{stackPlot} resets \code{y} values by applying 
   \code{y - min(y)} to each layer and then stacks them. It also generates a 
   second element \code{y0} of asociated baselines. This is then used in 
   the form \code{x = c(x, rev(x))}, \code{y = c(y, rev(y0))} with 
   \code{panel.polygon} to generate the stack layers.   

   \code{panel.stackPlot}: 

   Code currently in revision. Please handle with care.
   
              
}

\seealso{

   In \code{loa}: \code{\link{loaPlot}} and \code{\link{panelPal}}. 

   In other packages: \code{\link{xyplot}} and \code{\link{panel.polygon}}  in \code{\link{lattice}}. 

}

\examples{

## Example 1
## Basic stack plot usage

#stackPlot(lead~dist.m, data=lat.lon.meuse)
#stackPlot(cadmium+copper+lead+zinc~dist.m, data=lat.lon.meuse)

stackPlot(cadmium*40+copper*5+lead+zinc~dist.m, data=lat.lon.meuse)
 
}


\keyword{ methods }

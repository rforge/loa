\name{2.1.specialist.panels}
\alias{2.1.specialist.panels}
\alias{panel.kernelDensity}
\alias{panel.binPlot}

\title{
  Special panel functions 01
}
\description{
  In development specialist panel functions for use with lattice
}
\usage{

panel.kernelDensity(x, y, z = NULL, ..., n = 20, 
         kernel.fun = NULL, panel.range = TRUE, 
         process = TRUE, plot = TRUE, loa.settings = FALSE)

panel.binPlot(x = NULL, y = NULL, z = NULL, 
         breaks=20, x.breaks = breaks, y.breaks = breaks,
         x1=NULL, x2=NULL, y1=NULL, y2=NULL,
         statistic = mean, pad.grid = TRUE, ...,
         plot = TRUE, process = TRUE, loa.settings = FALSE 
         )

}

\arguments{

  \item{x, y, z}{ \code{lattice} function arguments passed down to 
   individual panels. 
   }

  \item{...}{ Additional arguments, typically passed on. See below.
  } 

 \item{n}{ For \code{panel.kernelDensity}, the number of x and y 
  cases to estimate when estimating density.  
  } 

 \item{kernel.fun}{ A function that can estimate kernel densities. 
  } 

 \item{panel.range}{ For \code{panel.kernelDensity}, a logical (default 
  \code{FALSE}) indicating if the kernel density estimation data range 
   should be forced to the full panel range. See Below   
  }

 \item{process, plot, loa.settings}{ For \code{panel...} functions that 
   intended to be handled using \code{\link{panelPal}}. \code{process} 
   and \code{plot}, logicals, indicating if the process and plot sections 
   of the panel function should be run. \code{loa.settings}, a logical 
   indicating if the safe mode setting should be returned. See below and 
   \code{\link{panelPal}} Help documents for further details. 
  }

 \item{breaks, x.breaks, y.breaks}{ For \code{panel.binPlot}, how many  
  break points to introduce when binning data. \code{breaks} can be used 
  to use the same number of breaks on both axes, while \code{x.breaks} and 
  \code{y.breaks} can be used to set these separately. 
 }

 \item{x1, x2, y1, y2}{ For \code{panel.binPlot}, vectors giving the 
  bin cell dimensions used when binning \code{x} and \code{y} elements. 
  Typically ignored and calculated within the plot call.
 }

 \item{statistic}{ For \code{panel.binPlot}, the function to use when 
  calculating \code{z} values for each set of binned. By default, this is 
  \code{mean}. So, if a \code{z} element is supplied in the plot call, the 
  data is binned according to \code{x} and \code{y} values, and the mean of 
  \code{z} values within each bin reported/plotted. If \code{z} is not 
  supplied, \code{statistic} is reset to \code{length} to generate a frequncy 
  plot and a warning generated.  
 }

 \item{pad.grid}{ For \code{panel.binPlot}, Logical, should empty bins be 
  reported? If \code{TRUE}, they are reported as \code{NAs}; if \code{FALSE}, 
  they are not reported.      
 }

} 


\details{
  Special \code{panel...} functions are intended for use with 
  \code{\link{panelPal}}. They provide alternative data handling 
   for \code{lattice} functions like \code{xyplot}. They are intended 
   to be used in the conventional form, e.g.:

   \code{xyplot(..., panel = panel.kernelDensity)}\cr
   \code{xyplot(..., n = 50, panel = panel.kernelDensity)}\cr
   \code{xyplot(..., panel = function(...) panel.kernelDensity(..., n = 50))}\cr
   \code{#etc}

   The combination provides a mechanism for the routine preprocessing of 
   panel data and the routine alignment panel and legend settings in cases 
   where values are reworked within the panel function call.

  \code{panel.kernelDensity} generates kernel density estimations 
   based on the supplied \code{x} and \code{y} data ranges.

}

\value{

  As with other \code{panel...} functions, output suitable for 
  use as the \code{panel} argument in \code{lattice} plot calls. 
   
}

\references{
  These function makes extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5
 
  (for \code{panel.kernelDensity}) MASS package:
  Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics 
  with S. Fourth edition. Springer. 

}

\author{
  Karl Ropkins
}

\note{

  \code{panel.kernelDensity} passes additional arguments on to the 
   \code{kernel.fun} to estimate kerenel density and the \code{lattice} 
   function \code{panel.contourplot} to generate the associated plot. 
   If no \code{kernel.fun} is supplied in the \code{panel} call, the 
   \code{MASS} function \code{kde2d} is used to estimate kernel density.  

}

\seealso{

   \code{\link{panelPal}}

   \code{\link{lattice}}, \code{\link{xyplot}}, \code{\link{panel.contourplot}},
}

\examples{

a <- rnorm(1000)
b <- rnorm(1000)

xyplot(a~b, panel = panel.kernelDensity)

#or with loaPlot
#loaPlot(~a*b, panel = panel.kernelDensity)

#etc.

}


\keyword{ methods }

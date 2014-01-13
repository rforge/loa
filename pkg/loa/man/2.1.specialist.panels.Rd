\name{2.1.specialist.panels}
\alias{2.1.specialist.panels}
\alias{panel.kernelDensity}
\alias{panel.binPlot}

\title{
  Special panel functions 01
}
\description{
  Specialist panel functions for use with lattice and loa plots.
}
\usage{

panel.kernelDensity(x, y, z = NULL, ..., n = 20, 
         kernel.fun = NULL, panel.range = TRUE, 
         process = TRUE, plot = TRUE, loa.settings = FALSE)

panel.binPlot(x = NULL, y = NULL, z = NULL, 
         breaks=20, x.breaks = breaks, y.breaks = breaks,
         x1=NULL, x2=NULL, y1=NULL, y2=NULL,
         statistic = mean, pad.grid = FALSE, ...,
         plot = TRUE, process = TRUE, loa.settings = FALSE)

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
   Some specialist \code{panel...} functions can be used be used with 
   conventional \code{lattice} functions like \code{xyplot}, e.g.:
 
  \code{xyplot(..., panel = panel.kernelDensity)}\cr
  \code{xyplot(..., n = 50, panel = panel.kernelDensity)}\cr
  \code{xyplot(..., panel = function(...) panel.kernelDensity(..., n = 50))}\cr
  \code{#etc}

   However, they are intended for use with \code{\link{panelPal}}. 
   The combination provides a mechanism for the routine preprocessing of 
   panel data, the association of specialist keys, and the routine alignment 
   of panel and legend settings in cases where values are reworked within the 
   panel function call.

   Typically, these are intended for use with \code{\link{panelPal}}, e.g.:

   \code{loaPlot(..., panel = panel.kernelDensity)}\cr
   \code{#etc}

  \code{panel.kernelDensity} generates kernel density estimations 
   based on the supplied \code{x} and \code{y} data ranges. It ignores 
   any supplied \code{z} information if supplied in the form:

   \code{loaPlot(z~x*y, ..., panel = panel.kernelDensity)} 

  \code{panel.binPlot} bins supplied \code{z} data according to \code{x} and 
   \code{y} values and associated break points (set by \code{break} arguments), 
   and then calculates the required statistic for each of these. By default, 
   this is \code{\link{mean}}, but alternative functions can be set using the 
   \code{statistic} argument. if no \code{z} values are supplied, as in

   \code{loaPlot(~x*y, ..., panel = panel.binPlot)} 
 
   ... \code{panel.binPlot} resets \code{statistic} to \code{\link{length}} and 
   gives a count of the number of elements in each bin.

   These \code{panel...} functions       
 
}

\value{

  As with other \code{panel...} functions in this package, output are suitable 
  for use as the \code{panel} argument in \code{loa} (and sometimes 
  \code{lattice}) plot calls. 
   
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

  Both \code{panel...} functions can color both regions and lines. For 
   \code{panel.kernelDensity} these are colored regions and contour lines 
   separating them. For \code{panel.binPlot} these are the individual data 
   bins and the borders surrounding them. In both cases the color scheme 
   applied to the colored regions ares controlled by \code{col.regions} and the 
   colors applied to the lines are controlled by \code{col}. 

  \code{panel.kernelDensity} passes additional arguments on to the 
   \code{kernel.fun} to estimate kerenel density and the \code{lattice} 
   function \code{panel.contourplot} to generate the associated plot. 
   If no \code{kernel.fun} is supplied in the \code{panel} call, the 
   \code{MASS} function \code{kde2d} is used to estimate kernel density.

  \code{panel.binPlot} passes limited arguments on to \code{lrect}.  

}

\seealso{

   In \code{loa}: \code{\link{panelPal}}

   In \code{\link{lattice}}: \code{\link{xyplot}}, \code{\link{panel.contourplot}},
   \code{\link{lrect}} 

}

\examples{

## Example 1
## Specialist kernel density panel example 

a <- rnorm(1000)
b <- rnorm(1000)
c <- rnorm(1000)

xyplot(a~b, panel = panel.kernelDensity, at = 0:5*5)

loaPlot(~a*b, panel = panel.kernelDensity)

#  Note 1:
#  at sets col.regions for the color surface, but, as this is calculated 
#  in-panel, this is not known at time of call. So, you need to set when 
#  using specialist panels with standard lattice plots. 
#  (Same is ture for any panel where plot attributes that are set in-panel 
#  and need to be known in all panels and keys for consistent output.)
#  loa panels include separate process and plot steps that panelPal can use  
#  to track these. 

#  Note 2:
#  By default, the panel ignores z data.  
#
#  compare: 
#  loaPlot(c~a*b, panel = panel.kernelDensity)        #where z term (c) ignored
#  loaPlot(c~a*b, panel = panel.kernelDensity, n=100) #finer surface resolution


## Example 2
## Specialist bin plot panel example 

#  By default, the panel bins supplied z case as mean
#  modify by supplying alternative as statistic

loaPlot(c~a*b, panel = panel.binPlot)
loaPlot(c~a*b, panel = panel.binPlot, statistic=max)

#  Note:
#  If z is not supplied, statistic defaults to length to give a count 
#
#  loaPlot(~a*b, panel = panel.binPlot)               #where z term not supplied

#etc.

}


\keyword{ methods }

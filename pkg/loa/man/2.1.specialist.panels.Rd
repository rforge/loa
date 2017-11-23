\name{2.1.specialist.panels}
\alias{2.1.specialist.panels}
\alias{panel.kernelDensity}
\alias{panel.binPlot}
\alias{panel.surfaceSmooth}
\alias{panel.loaLevelPlot}

\title{
  Special panel functions 01
}
\description{
  Specialist panel functions for use with lattice and loa plots.
}
\usage{


panel.loaLevelPlot(x = NULL, y = NULL, z = NULL, ..., 
         loa.settings = FALSE)

panel.surfaceSmooth(x = NULL, y = NULL, z = NULL, 
         breaks = 200, x.breaks = breaks, y.breaks = breaks, 
         smooth.fun = NULL, too.far=0, ..., 
         plot = TRUE, process = TRUE, loa.settings = FALSE)

panel.kernelDensity(x, y, z = NULL, ..., n = 20, 
         local.wt = TRUE, kernel.fun = NULL, too.far = 0, 
         panel.range = TRUE, process = TRUE, plot = TRUE, 
         loa.settings = FALSE)

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

 \item{loa.settings, process, plot}{ For \code{panel...} functions that 
   intended to be handled using \code{\link{panelPal}}. \code{loa.settings}, 
   a logical indicating if the safe mode setting should be returned. 
   \code{process} and \code{plot}, logicals, indicating if the process and 
   plot sections of the panel function should be run. See below and 
   \code{\link{panelPal}} help documents for further details. 
  }

 \item{breaks, x.breaks, y.breaks}{ (For \code{panel.surfaceSmooth} and 
  \code{panel.binPlot}) How many break points to introduce when smoothing 
   a surface or binning data. \code{breaks} can be used to set the same number 
   of breaks on both axes, while \code{x.breaks} and \code{y.breaks} can be 
   used to set these separately. 
 }

 \item{smooth.fun}{ (For \code{panel.surfaceSmooth}) A function that can fit 
  a surface estimate to \code{(x,y,z)} data. See notes below for further 
  details.
  } 

 \item{too.far}{ (For \code{panel.surfaceSmooth} and \code{panel.kernelDensity}) The distance from original data 
  at which to stop predicting surface values. See notes below for further 
  details.  
  }

 \item{n}{ (For \code{panel.kernelDensity}) the number of x and y 
  cases to estimate when estimating density.  
  } 

 \item{local.wt}{ (For \code{panel.kernelDensity}) A logical (default \code{TRUE})
  indicating if kernel density estimates should be weighed relative to other 
  groups, panels, etc.  
  } 

 \item{kernel.fun}{ (For \code{panel.kernelDensity}) A function that can 
  estimate kernel densities. 
  } 

 \item{panel.range}{ (For \code{panel.kernelDensity}) A logical (default 
  \code{FALSE}) indicating if the kernel density estimation data range 
   should be forced to the full panel range. See Below.   
  }

 \item{x1, x2, y1, y2}{ (For \code{panel.binPlot}) Vectors giving the 
  bin cell dimensions used when binning \code{x} and \code{y} elements. 
  Typically ignored and calculated within the plot call.
 }

 \item{statistic}{ (For \code{panel.binPlot}) the function to use when 
  calculating \code{z} values for each set of binned. By default, this is 
  \code{mean}. So, if a \code{z} element is supplied in the plot call, the 
  data is binned according to \code{x} and \code{y} values, and the mean of 
  \code{z} values within each bin reported/plotted. If \code{z} is not 
  supplied, \code{statistic} is reset to \code{length} to generate a frequency 
  plot and a warning generated.  
 }

 \item{pad.grid}{ For \code{panel.binPlot}, Logical, should empty bins be 
  reported? If \code{TRUE}, they are reported as \code{NAs}; if \code{FALSE}, 
  they are not reported.      
 }





} 


\details{

   \code{panel.loaLevelPlot} is intended for plot data structured 
   for use with the \code{\link{lattice}} function \code{\link{levelplot}}, 
   e.g. regularised \code{(x,y,z)} or a \code{matrix}:

   \code{loaPlot(..., panel = panel.loaLevelPlot)}\cr
   \code{levelplot(...) #in lattice}\cr
    
   Other specialist \code{panel...} functions can be used with the 
   \code{\link{lattice}} function \code{\link{xyplot}}:
 
   \code{xyplot(..., panel = panel.kernelDensity)}\cr
   \code{xyplot(..., n = 50, panel = panel.kernelDensity)}\cr
   \code{xyplot(..., panel = function(...) panel.kernelDensity(..., n = 50))}\cr
   \code{#etc}

   However, they are intended for use with \code{loa} plots that incorporate
   \code{\link{panelPal}}. This combination provides a mechanism for the 
   routine preprocessing of panel data, the association of specialist keys, 
   and the routine alignment of panel and legend settings in cases where 
   values are reworked within the panel function call:
 
   \code{loaPlot(..., panel = panel.kernelDensity)}\cr
   \code{#etc}

   \code{panel.surfaceSmooth} and other similar \code{panel...} functions 
   generate smoothed surfaces using supplied \code{(x,y,z)} data and pass 
   this to \code{panel.loaLevelPlot} to plot.

   By default, \code{panel.surfaceSmooth} uses \code{stats} function 
   \code{\link{loess}} to generate a surface. Alternative smooths 
   can be set using the \code{smooth.fun} argument, and the surface 
   range can to controlled using the \code{too.far} argument. 
    
  \code{panel.kernelDensity} generates kernel density estimations 
   based on the supplied \code{x} and \code{y} data ranges. Because it is 
   density plot, it counts the number of \code{z} values. So, \code{z} values 
   are ignored. It is intended to be used in the form:

   \code{loaPlot(~x*y, ..., panel = panel.kernelDensity)} 

   So, if any \code{z} information is supplied, users are warned that it has 
   been ignored, e.g: 

   \code{loaPlot(z~x*y, ..., panel = panel.kernelDensity)}\cr
   \code{#warning generated}

   \code{panel.binPlot} bins supplied \code{z} data according to \code{x} and 
   \code{y} values and associated break points (set by \code{break} arguments), 
   and then calculates the required statistic for each of these. By default, 
   this is \code{\link{mean}}, but alternative functions can be set using the 
   \code{statistic} argument. It is intended to be used in form:

   \code{loaPlot(z~x*y, ..., panel = panel.binPlot)} 

   If no \code{z} values are supplied, as in:

   \code{loaPlot(~x*y, ..., panel = panel.binPlot)} 
 
   ... \code{panel.binPlot} resets \code{statistic} to \code{\link{length}} 
   (again with a warning) and gives a count of the number of elements in 
   each bin.
 
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

  (for \code{panel.surfaceSmooth}) mgcv package and too.far argument:
  Wood, S.N. (2004) Stable and efficient multiple smoothing parameter
  estimation for generalized additive models.Journal of the American
  Statistical Association. 99:673-686.
  Also http://www.maths.bath.ac.uk/~sw283/

}

\author{
  Karl Ropkins
}

\note{

   All these \code{panel...} functions treat \code{col} and \code{col.regions}, 
   etc, as discrete arguments. Typically, \code{col} links to lines (contour 
   lines for surfaces, bin borders for binned data) and \code{col.regions} 
   links any generates surface region. 
   
   \code{panel.surfaceSmooth} passes additional arguments on to the 
   \code{smooth.fun} to estimate surface smooths and the \code{lattice} 
   function \code{panel.levelplot} to generate the associated plot. 
   If no \code{kernel.fun} is supplied in the \code{panel} call, the 
   \code{stats} function \code{loess} is used to estimate surface smooth.
   The \code{too.far} argument is based on same in \code{\link{vis.gam}} 
   function in the \code{mgcv} package.

  \code{panel.kernelDensity} passes additional arguments on to the 
   \code{kernel.fun} to estimate kerenel density and the \code{lattice} 
   function \code{panel.contourplot} to generate the associated plot. 
   If no \code{kernel.fun} is supplied in the \code{panel} call, the 
   \code{MASS} function \code{kde2d} is used to estimate kernel density.

  \code{panel.binPlot} passes limited arguments on to \code{lrect}.

  \code{panel.kernelDensity} and \code{panel.binPlot} are currently under 
   review. 

}

\seealso{

   In \code{loa}: \code{\link{panelPal}}

   In \code{\link{lattice}}: \code{\link{xyplot}}, \code{\link{levelplot}}, 
   \code{\link{panel.contourplot}}, \code{\link{lrect}} 

}

\examples{

## Example 1
## for data already set up for levelplot

loaPlot(volcano, panel=panel.loaLevelPlot)

## Example 2
## Surface smooth

loaPlot(copper~longitude*latitude, data= lat.lon.meuse, 
        panel=panel.surfaceSmooth, grid=TRUE, 
        too.far=0.1, col.regions=3:2)

}


\keyword{ methods }

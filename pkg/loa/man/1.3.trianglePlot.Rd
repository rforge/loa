\name{1.3.trianglePlot}
\alias{1.3.triangle.plots}
\alias{trianglePlot}
\alias{panel.trianglePlot}
\alias{panel.triangleKernelDensity}
\alias{panel.triangleSurfaceSmooth}
\alias{panel.trianglePlotFrame}
\alias{panel.trianglePlotAxes}
\alias{panel.trianglePlotGrid}
\alias{triLimsReset}
\alias{triABC2XY}
\alias{triXY2ABC}


\title{
  Triangle plots
}

\description{
  Triangle plot functions for Lattice. 
}

\usage{

   trianglePlot(x, data = NULL, ..., ref.cols = TRUE)

   #standard panels

   panel.trianglePlot(x = NULL, y = NULL, a0 = NULL, b0 = NULL, 
            c0 = NULL, ..., loa.settings = FALSE, plot = TRUE, 
            process = TRUE)
   panel.trianglePlotFrame(...,  grid = NULL, axes = NULL)
   panel.trianglePlotGrid(alim = NULL, blim = NULL, clim = NULL, 
            ..., grid = TRUE, panel.scales = NULL)
   panel.trianglePlotAxes(alim = NULL, blim = NULL, clim = NULL, 
            ..., axes = TRUE, ticks=TRUE, annotation=TRUE, 
            panel.scales = NULL)  

   #other panels

   panel.triangleKernelDensity(x = NULL, y = NULL, a0 = NULL, 
            b0 = NULL, c0 = NULL, ..., loa.settings = FALSE, 
            plot = TRUE, process = TRUE) 
   panel.triangleSurfaceSmooth(x = NULL, y = NULL, z = NULL, 
            a0 = NULL, b0 = NULL, c0 = NULL, ..., loa.settings = FALSE, 
            plot = TRUE, process = TRUE) 

   #data handlers

   triABC2XY(a, b = NULL, c = NULL, ..., force.abc = TRUE, 
            if.na = "remove.row", if.neg = "remove.row", 
            verbose = FALSE)
   triXY2ABC(x, y = NULL, ..., force.xy = TRUE, 
            verbose = FALSE)
   triLimsReset(ans)
}

\arguments{

  \item{x}{ For \code{trianglePlot} only, a formula with the general structure 
   \code{z ~ a0 + b0 + c0 | cond}. The elements a0, b0 and c0, the inputs for 
   the three axis on the triangle plot, are required, while \code{z} 
   and conditioning (\code{cond}) are optional. (For other functions, \code{x} 
   may be used as the pair to \code{y}. See \code{y} below.)  
   }

  \item{data}{ For \code{trianglePlot} only, if supplied, the assumed source 
   of the elements of formula \code{x}, typically a \code{data.frame}.
   }

  \item{...}{ Additional arguments. 
   }

  \item{ref.cols}{ Either a logical to turn off/on grid color-coding or a 
   vector of colors to be applied to a0, b0 and c0 axes and grids. These are 
   applied to the grid lines and axes tick and annotation components. Some users, 
   particularly those less familiar with triangle plots, can find such color 
   referencing helpful when analyzing such plots. By default, the colorings are 
   quite subtle, so users can see the effect if them look for it but it does not 
   take over the plot when it is not focused on. Finer control can be achieved using 
   \code{axes}, \code{ticks}, \code{grid}, etc. (See below).  
   }

  \item{y, a, a0, b, b0, c, c0, z}{(and \code{x} in relevant functions). \code{a/a0} , \code{b/b0} and 
   \code{c/c0} are the three scales of the triangle plot, and \code{x} and \code{y} 
   are the equivalent 2-D projections. The arguments are typically options in panel functions 
   (\code{panel...} functions), conversion functions (\code{triABC2XY} and \code{triXY2ABC}) and 
   the scaling function \code{triLimsReset}. \code{z} is the z-case from the plot formula.  
   }

  \item{loa.settings, plot, process}{\code{\link{loaPlot}} arguments used to manage 
   \code{\link{panelPal}} activity.
   }  

  \item{grid, axes, ticks, annotation}{ User-resets for the axes, grid, tick and annotation 
   elements of the plots. These can be \code{NULL} or \code{FALSE} to turn off, \code{TRUE} to show, 
   a vector (in which case they are assumed to be color assignments) or a list of standard plot 
   parameters, e.g. \code{col}, \code{lty}, \code{lwd}, etc. for color, line type and line thickness, 
   etc. Plot parameter assignments are applied to all axes unless specific axes are identified. For 
   example, \code{trianglePlot} calls including \code{grid.col = 2} or \code{grids = list(col = 2)} make 
   all axes red, while calls including \code{grid.a0.col = 2} or \code{grid = list(a0 = list(col = 2)} only 
   recolor the first (a0) axis.
   }

  \item{alim, blim, clim}{ Delimiters for a, b and c scales, equivalent to \code{xlim} 
    and \code{ylim} in conventional plots, but less flexible. See Details below for 
    more information.
   }

   \item{panel.scales}{ A local argument, typically a list, that controls the 
    appearance of the a0/b0/c0 axes. This is roughly equivalent to the \code{scales} 
    argument used by conventional \code{lattice} plots to handle x and y axis, 
    but intended for non-standard scales, such as the triangle axes used here. 
    It can be set directly or used in combination with the local scale(s) handler 
    function \code{\link{localScalesHandler}} to override/hijack standard scales 
    operations. (See note below).
   }

  \item{force.abc, force.xy}{Logicals. If a list or data.frame is supplied 
   to \code{triABC2XY} or \code{triXY2ABC} as a source or a/b/c or x/y respectively 
   should appropriately named elements be used regardless of order? See Note below.
  } 

  \item{if.na}{ Character. Handling method to be used if \code{NAs} are present. 
   The default \code{'remove.row'} replaces all enteries in the same row with 
   \code{NAs}. (Note: this is different from \code{\link{na.omit}} which would 
   remove the whole row. Here, the row is retained as \code{NAs} to maintain 
   indices for conditioning.) Other options currently include: \code{'make.zero'} 
   to replace the \code{NA} with \code{0}; and \code{'keep.as.is'} to leave 
   unchanged. 
  } 

  \item{if.neg}{ Character. Like \code{if.na} but for negative values: 
   \code{'remove.row'} to replace all enteries in the same row with 
   \code{NAs}; \code{'make.zero'} to replace all negative values with 
   \code{0}; \code{'rescale.col'} recales any column (i.e., \code{a}, \code{b} or 
   \code{c}) that contains a negative from zero by subtracting the minimum.
  } 

  \item{verbose}{ Logical, default \code{FALSE}. Should a full output be returned? 
   The alternative \code{FALSE} generates a minimal report. 
  } 

  \item{ans}{For \code{triLimsReset} only, a trellis ouput, e.g. a \code{lattice} plot, 
   to be scaled and plotted based on the assumption that it is a \code{trianglePlot}.
  }

  
}


\details{

   \code{trianglePlot} generates a triangle plot using the lattice framework.

   \code{panel.trianglePlot...} functions handle the appearance of triangle plot
    outputs. \code{panel.trianglePlot}, which is assigned as the default \code{panel} 
    manages both the data layer of the plot and the plot frame (axes, grid, annotation, 
    etc). \code{panel.trianglePlotAxes} and \code{panel.trianglePlotGrid} generate 
    axes and grid componets of the plot, and \code{panel.trianglePlotFrame} is 
    a wrapper for these. The data layer, which by default is \code{panel.loaPlot}, 
    can be accessed separately using the \code{data.panel} argument. 

   \code{triangleKernelDensity} generates a kernel density surface for supplied \code{a0}, 
   \code{b0} and \code{c0} cases. 

   \code{triangleSurfaceSmooth} generates a smoothed surface for supplied \code{a0}, 
   \code{b0}, \code{c0} and \code{z} cases. 

   \code{triABC2XY} converts supplied (a, b, c) coordinates to an (x, y) scale suitable 
    for use with \code{panel.trianglePlotFrame}, etc. 

    \code{triXY2ABC} converts supplied (x,y) coordinates from triangle plots to the associated 
    proportional (a, b, c) scale. 

    There are various options for range limiting with \code{triABC2XY} and \code{triXY2ABC}, 
    and therefore \code{trianglePlot} as well. Firstly, limits can be set individually with 
    \code{alim}, \code{blim} and \code{clim}, much like with \code{xlim} and \code{ylim} for 
    conventional plots. However, they can also be set at once using \code{lims}, as in e.g. 
    \code{lims = c(0, 1)} to set all axes to full ranges, or on the basis of minimum and maximum 
    cut-offs using \code{abc.mins} and \code{abc.maxs}, respectively.    

    \code{trianglePlot} uses \code{\link{localScalesHandler}} to override normal 
    \code{lattice} handling of \code{scales}. This allows parameters for axes other 
    than \code{'x'} and \code{'y'} to be passed via the \code{scales} argument for 
    axis generation within the plot panel itself. The function does this by recovering 
    the information for each of the local axes (here \code{a0}, \code{b0} and \code{c0}) 
    from \code{scales}, and passing this on to the plot as the argument 
    \code{panel.scales} which can then be evaluated by an appropriate \code{panel...} 
    function like \code{panel.trianglePlotAxes}. At the same time it also resets 
    \code{scales} to stop the standard axes being generated. The intention here is two-fold. 
    Firstly, to provide plot users with an axes control mechanism like the standard 
    \code{scales} control of \code{x} and \code{y} that they already know. And, secondly, 
    to provide developers with a simple framework for the quick addition of non-standard axes 
    or scales. See \code{\link{localScalesHandler}} and \code{\link{panel.localScale}} for 
    further details.
     
    \code{trianglePlot} uses \code{\link{getPlotArgs}} to manage \code{lattice} defaults and 
    plot developer and user resets for the different plot components (axes, ticks, grid, 
    annotation). As with \code{localScalesHandler}, the intention here is to provide more 
    routine access to higher level plot control.    

} 


\value{

   \code{trianglePlot} returns trellis objects, much like 
   conventional \code{lattice} plot functions.

   \code{panel.trianglePlot...} functions are intended for use 
   within a \code{trianglePlot} function call. 

   \code{triABC2XY} returns a list containing the named 
   components \code{x} and \code{y}, which are the 2-D (x,y) transformations 
   of supplied (a,b,c) \code{trianglePlot} elements.

   \code{triXY2ABC} returns a list containing the named 
   components \code{a}, \code{b} and \code{c}, which are the (a,b,c) 
   \code{trianglePlot} coordinates associated with supplied 2-D (x, y) that 
   \code{trianglePlot} would generate.

   \code{resetTriLims} returns a supplied trellis object, rescaled 
   based on the assumption that it is a \code{trianglePlot}.

}

\references{
  These function makes extensive use of code developed by others.  

  Currently, several triangle plotting methods are under evaluation for use within 
  this package. These are: 

  The tri-plot method of Graham and Mudgley:

  Graham, David J. and Mudgley, Nicholas, G. Graphical representation of particle 
  shape using triangular diagrams: An Excel spreadsheet method. Earth Surface 
  Processes and Landforms, 25, 1473-1477, 2000.

  The triangle.param method of Chessel (as coded in R package 'ade4')

  Dray, S. and Dufour, A.B.(2007). The ade4 package: implementing the 
  duality diagram for ecologists. Journal of Statistical, Software. 22(4): 1-20.

  Chessel, D. and Dufour, A.B. and Thioulouse, J. (2004). The ade4 package - I - 
  One-table methods. R News. 4: 5-10.

  Dray, S. and Dufour, A.B. and  Chessel, D. (2007). The ade4 package-II: Two-table 
  and K-table methods. R News. 7(2): 47-52.

  And the trilinear plot of Allen as reported by Zhu:
  
  Zhu, M. (2008). How to draw a trilinear Plot. Statistical Computing & Graphics, 19(1), 
  June, 2008.  

  In this version of the package tri-plot methods are used for the \code{triABC2XY} and 
  \code{triXY2ABC} transforms and a modification \code{triangle.param} methods is used 
  to calculate suitable values for \code{alim}, \code{blim} and \code{clim}.

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

   General: 

   With triangle plots, the (a0, b0, c0) scales are proportional. So regardless of the 
   absolute sizes of a coordinate set (a,b,c), values are plotted and handled 
   as proportions, i.e. a/(a+b+c), b/(a+b+c) and c/(a+b+c), respectively. This means 
   that absolute values of \code{a}, \code{b} and \code{c} are lost when points are 
   plotted on these axes. So, the function \code{triXY2ABC} returns the relative proportions 
   of \code{a}, \code{b} and \code{c}, not the absolute amounts, when translating a 2-D 
   (x,y) coordinates into the associated (a, b, c) coordinates.  

   Development:

   This is an in-development plot, and 'best handling' strategies have not been decided for 
   several elements. So, future versions of these functions may differ significantly from the 
   current version. 

   In particular:

   Current axes assignments, e.g. (a, b, c) versus (a0, b0, c0), etc., have not be finalised. 
   So, these may change in future versions of the function. 
   
   Currently, \code{trianglePlot} scale adjustment options have been limited. Options under 
   evaluation include: (1) by \code{alim}, \code{blim}, \code{clim} setting, equivalent to 
   \code{xlim} and \code{ylim}, (2) by \code{lims} to set all axes ranges the same, and 
   (3) by \code{maxs} to setting all axes range maximums and \code{mins} to set all axes 
   range minimums, etc. 

   These options are currently only avialable via the data converters. 

   One of the issues here is that the axes ranges are all inter-linked. The range of one axes 
   is a function of the other two axes ranges. So, setting these can generate contradictions. 
   For example, \code{lims=c(0,0.1)} should in theory set all ranges to (0, 0.1). But, the triangle 
   \code{a = b = c = c(0, 0.1)} cannot exist. Therefore, the plot would attempt to recover 
   the extended range that includes all the requested ranges (\code{a = c(0, 0.1)}, 
   \code{b = c(0, 0.1)}  and \code{c = c(0, 0.1)}), which in this case is the full 
   range: \code{a = b = c = c(0, 1)}. Suggestions on this topic are very welcome. 

   \code{trianglePlot}:

   As part of the \code{loa} version 0.2.19 update, \code{trianglePlot} was rewritten to run 
   with the most recent version of \code{\link{panelPal}} function. This means all plot 
   functions in \code{loa} now use the most recent version of \code{\link{panelPal}}. 

   This update should provide improved plot handling similar to recent versions of \code{\link{loaPlot}} 
   and \code{\link{GoogleMap}} functions which both already (from versions 0.2.0 onwards) use the latest 
   version of \code{panelPal}.

   \code{panel.trianglePlotAxes}: 

   Code currently in revision. Please handle with care.
   
   \code{triABC2XY}: 

   Code currently in revision. Please handle with care.
              
}

\seealso{

   In \code{loa}: For in-panel axis/scale generation, see \code{\link{loaPlot}}, \code{\link{panelPal}}, 
   \code{\link{localScalesHandler}} and \code{\link{panel.localScale}}. 

   In other packages: \code{\link{xyplot}} in \code{\link{lattice}}. 

}

\examples{

## Example 1
## Basic triangle plot usage

trianglePlot(cadmium~copper+lead+zinc|lime, 
             data=lat.lon.meuse)

# Notes:
# Formula structure z ~ a0 + b0 + c0 |cond, where a0, b0 and 
# c0 are the three axes of the triangle plot 
# Data (and groups) assignment like in standard lattice plots.
# By default z is linked to col and cex. 
# Unless overridden by user inputs or group or zcase setting. 
# Plot handling is similar to loaPlot 
# (So, see ?loaPlot and ?panelPal for further details.)

# Formula variations:
# basic triangle plot without z values assignment
# trianglePlot(~copper+lead+zinc, data=lat.lon.meuse) 
# ... with z values set
# trianglePlot(cadmium~copper+lead+zinc, data=lat.lon.meuse)
# ... with grouping 
# trianglePlot(cadmium~copper+lead+zinc, groups = lat.lon.meuse$lime, 
#              data=lat.lon.meuse)


## Example 2 
## Basic frame (axes, grid, tick, annotation) management 

# trianglePlot(~1+1+1, type="n")          ## empty frame
                                          ## make using type="n"    
# trianglePlot(~1+1+1, type="n", 
#              grid = FALSE)              ## turn off grid 
# trianglePlot(~1+1+1, type="n", 
#              grid.col = 2)              ## or change plot settings
# trianglePlot(~1+1+1, type="n", 
#              grid.a0.lty = 1)
# trianglePlot(~1+1+1, type="n", 
#              grid = list(a0 = list(lty = 1)))

  trianglePlot(~1+1+1, type="n", 
               grid.alpha = 0.2, 
               ticks.alpha = 0.2)         ## grid and tick alpha reset

# notes:
# Here, grid and ticks arguments are used to remove or modify these 
# elements of the plot frame individually.  
# Setting can be management in list form like in normal lattice or 
# in a loa shorthand where e.g. the argument grid.a0.lty = 1 is equivalent 
# to grid = list(a0 = list(lty = 1))
# (So, quicker if you are only changing a small number of elements.)
 
}


\keyword{ methods }

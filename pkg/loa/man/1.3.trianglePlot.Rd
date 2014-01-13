\name{1.3.trianglePlot}
\alias{1.3.triangle.plots}
\alias{trianglePlot}
\alias{panel.triangleFrame}
\alias{panel.triangleAxes}
\alias{panel.triangleGrids}
\alias{triABC2XY}
\alias{triXY2ABC}


\title{
  Triangle plots for lattice
}

\description{
  Triangle plot functions for lattice
}

\usage{

trianglePlot(x, data = NULL, panel = panel.xyplot, 
          ref.grids = TRUE, ..., axes = TRUE, ticks = TRUE, 
          grids = TRUE, annotation = TRUE)

#associated panels
panel.triangleFrame(x = NULL, y = NULL, a = NULL, b = NULL, 
          c = NULL, ..., grids = grids, user.panel = panel.xyplot)
panel.triangleAxes(alim = NULL, blim = NULL, clim = NULL, ..., 
          panel.scales = panel.scales, axes = TRUE, 
          ticks = TRUE, annotation = TRUE)
panel.triangleGrids(a.grid = NULL, b.grid = NULL, c.grid = NULL, 
          ..., alim = NULL, blim = NULL, clim = NULL,  
          panel.scales = NULL, grids = TRUE)

#data handlers
triABC2XY(a, b = NULL, c = NULL, ..., force.abc = TRUE, 
          if.na = "remove.row", if.neg = "remove.row", 
          verbose = FALSE)
triXY2ABC(x, y = NULL, ..., force.xy = TRUE, 
          verbose = FALSE)
}

\arguments{

  \item{x}{ For \code{trianglePlot}, a formula with the general structure 
   \code{z ~ a + b + c | cond}. The elements a, b and c, the inputs for 
   the three axis on the triangle plot, are required, while \code{z} 
   and conditioning \code{cond} are optional. For other functions, 
   \code{x} is also used as the pair to \code{y} for a/b/c to x/y 
   conversion and conventional x/y plotting of rescaled a/b/c 
   data. (See \code{y} below.)  
   }

  \item{data}{ (\code{trianglePlot} only) If supplied, the assumed source 
   of the elements of formula \code{x}, typically a \code{data.frame}.
   }

  \item{panel, user.panel}{ \code{panel} is the panel to use when plotting the 
   data layer of \code{trianglePlot} outputs. By default this is the 
   \code{lattice} function \code{panel.xyplot}. To handle the a/b/c to 
   x/y conversion (and allow similar handling of other \code{lattice} panels like 
   \code{panel.lines} and \code{panel.polygon}) and axes generation, \code{trianglePlot} 
   passes this to \code{panel.triangeFrame} as \code{user.panel}.  
   }

  \item{ref.grids}{ Either a logical to turn off/on grid reference colors or a 
   vector of colors to be applied to a, b and c axes, respectively. These are 
   applied to the grid lines and axes tick and annotation components. Some users, 
   particularly those less familiar with triangle plots, can find such color 
   referencing helpful when analyzing such plots. By default, the colorings are 
   quite subtle, so users can see the effect if them look for it but it does not 
   take over the plot if it is not wanted. Finer control can be achieved using 
   \code{axes}, \code{ticks}, \code{grids} and \code{annotation}. (See below).  
   }

  \item{...}{ Additional arguments. 

   For \code{trianglePlot}, these are passed to associated data handlers 
   (\code{\link{triABC2XY}}, \code{\link{cexHandler}}, \code{\link{colHandler}}, 
   etc), the axis handler \code{\link{localScalesHandler}} and the 
   \code{\link{lattice}} plot function \code{\link{xyplot}}. Arguments are  
   also passed via \code{\link{xyplot}} a panel function (by 
   default \code{panel.triangleFrame} which manages the data, axes and 
   grid components of the plot appearance.

   With other functions these are generally passed on but ignored.
   } 

  \item{axes, ticks, grids, annotation}{ user resets for the axes, axis tick marks, grid lines 
   axis annotation, respectively. These can be \code{NULL} or \code{FALSE} to turn off, 
   \code{TRUE} to show, a vector (in which case they are assumed to be color assignments) or 
   a list of standard plot parameters, e.g. \code{col}, \code{lty}, \code{lwd}, etc. for color, 
   line type and line thickness, etc. Plot parameter assignments are applied to all axes unless 
   specific axes are identified. For example, \code{trianglePlot} calls including \code{grids = 1} 
   or \code{grids = list(col = 1)} make all axes black, while calls including 
   \code{grids = list(a = list(col = 1)} only recolor the a-axis.
   }

  \item{y, a, b, c}{(and \code{x} in relevant functions). \code{a}, \code{b} and 
   \code{c} are the three scales of the triangle plot, and \code{x} and \code{y} 
   are the equivalent 2-D projections. 
   }

  \item{panel.scales}{ A local argument, typically a list, that controls the 
    appearance of the a/b/c axes. This is roughly equivalent to the \code{scales} 
    argument used by conventional \code{lattice} plots to handle x and y axis, 
    but intended for non-standard scales, such as the triangle axes used here. 
    It can be set directly or used in combination with the local scale(s) handler 
    function \code{\link{localScalesHandler}} to override/hijack standard scales 
    operations. (See note below).
   }

  \item{alim, blim, clim}{ delimiters for a, b and c scales, equivalent to \code{xlim} 
    and \code{ylim} in conventional plots, but less flexible. See Details below for 
    more information.
   }

  \item{a.grid, b.grid, c.grid}{ If supplied the positions of the a, b and c grid lines, 
    respectively.
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

}


\details{

   \code{trianglePlot} generates a triangle plot using the lattice framework.

   \code{panel.triangle...} functions handle the appearance of triangle plot
    outputs. \code{panel.triangleAxes} and \code{panel.triangleGrids} generate 
    a/b/c axes and grids, and \code{panel.triangleFrame} is a wrapper for these 
    and the \code{panel} function supplied by the user. \code{panel.triangleFrame} also 
    handles data scaling, so either (a, b, c) or appropriately rescaled (x, y) 
    combinations can be supplied. 

   \code{triABC2XY} converts supplied (a, b, c) coordinates to an (x, y) scale suitable 
    for use with \code{panel.triangleFrame}, etc. \code{triXY2ABC} converts supplied (x,y) 
    coordinates from triangle plots to the associated proportional (a, b, c) scale. 

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
    the information for each of the local axes (here \code{a}, \code{b} and \code{c}) 
    from \code{scales}, and passing this on to the plot as the argument 
    \code{panel.scales} which can then be evaluated by an appropriate \code{panel...} 
    function like \code{panel.triangleAxes}. At the same time it also resets 
    \code{scales} to stop the standard axes being generated. The intention here is two-fold. 
    Firstly, to provide plot users with an axes control mechanism like the standard 
    \code{scales} control of \code{x} and \code{y} that they already know. And, secondly, 
    to provide developers with a simple framework for the quick addition of non-standard axes 
    or scales. See \code{\link{localScalesHandler}} and \code{\link{panel.localScale}} for 
    further details.
     
    \code{trianglePlot} uses \code{\link{getPlotArgs}} to manage \code{lattice} defaults and 
    plot developer and user resets for the different plot components (axes, ticks, grids, 
    annotation). As with \code{localScalesHandler}, the intention here is to provide more 
    routine access to higher level plot control.    

} 


\value{

   \code{trianglePlot} returns trellis objects, much like 
   conventional \code{lattice} plot functions.

   \code{panel.triangle...} functions are intended for use 
   within a \code{trianglePlot} function call. With careful 
   handling, they may also be used in conventional \code{lattice} 
   plot calls. 

   \code{triABC2XY} returns a list containing the named 
   components \code{x} and \code{y}, which are the 2-D (x,y) equivalents 
   of supplied (a,b,c) \code{trianglePlot} elements.

   \code{triXY2ABC} returns a list containing the named 
   components \code{a}, \code{b} and \code{c}, which are the (a,b,c) 
   \code{trianglePlot} coordinates associated with supplied 2-D equivalents 
   (x, y).

}
\references{
  These function makes extensive use of code developed by others.  

  Currently, several triangle plotting methods are under evaluation for use within 
  this pacakge. These are: 

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
  
  Zhu, M. (2008). How to draw a trilinear Plot. Statistical computing & Graphics, 19(1), 
  June, 2008.  

  In this version of the package tri-plot methods are used for the \code{triABC2XY} and 
  \code{triXY2ABC} transforms and a modification \code{triangle.param} methods is used 
  to calculate suitable values for \code{alim}, \code{blim} and \code{clim}.

  And as elsewhere, the use of \code{lattice} is gratefully acknowledged

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

   With triangle plots, the (a, b, c) scales are proportional. So regardless of the 
   absolute sizes of a coordinate set (a,b,c), values are plotted and handled 
   as proportions, i.e. a/(a+b+c), b/(a+b+c) and c/(a+b+c), respectively. This means 
   that absolute values of \code{a}, \code{b} and \code{c} are lost when points are 
   plotted on these axes. So, the function \code{triXY2ABC} give the relative proportions 
   of \code{a}, \code{b} and \code{c}, not the absolute amounts, when translating a 2-D 
   (x,y) point into the associated (a, b, c) coordinate.  

   Development:

   This is an in-development plot, and 'best handling' strategies has not been decided for 
   several elements. So, future versions of these functions may differ significantly from the 
   current version. 

   In particular:

   Currently, \code{trianglePlot} provides scale adjustment options by \code{alim}, 
   \code{blim}, \code{clim} setting. Other options are also being evaluated, e.g. 
   \code{lims} to set all axes ranges the same, \code{maxs} to setting all axes range 
   maximums, \code{mins} to set all axes range minimums, etc. One of the issues here is 
   that the axes ranges are all inter-linked. The range of one axes is a function of the 
   other two axes ranges. So, setting these can generate contradictions. For example, 
   \code{lims=c(0,0.1)} should in theory set all ranges to (0, 0.1). But, the triangle 
   \code{a = b = c = c(0, 0.1)} cannot exist. Therefore, the plot would attempt to recover 
   the extended range that includes all the requested ranges (\code{a = c(0, 0.1)}, 
   \code{b = c(0, 0.1)}  and \code{c = c(0, 0.1)}), which in this case is the full 
   range: \code{a = b = c = c(0, 1)}. Any suggestions on this topic are very welcome. 

   \code{trianglePlot}:

   The \code{\link{panelPal}} function was extensively revised as part of the 0.1 -> 0.2 
   package version update. The current version of \code{trianglePlot} still uses the old version 
   of \code{panelPal} (\code{panelPal.old}). The migration of \code{trianglePlot} to the latest 
   version of \code{panelPal} is on the current package 'to-do'. 

   This update should provide improved plot handling similar to the current versions of \code{\link{loaPlot}} 
   and \code{\link{GoogleMap}} which both already use the new version of \code{panelPal}.

   \code{panel.triangleAxes}: 

   Code currently in revision. Please handle with care.
   
   \code{triABC2XY}: 

   Code currently in revision. Please handle with care.
              
}

\seealso{

   In \code{loa}: For in-panel axis/scale generation, see \code{\link{localScalesHandler}} and 
   \code{\link{panel.localScale}}. 

   In other packages: \code{\link{xyplot}} in \code{\link{lattice}}. 

}

\examples{

## Example 1
## trianglePlot Examples

#  lat.lon.meuse plots

#  Cu/Pb/Zn triangle plot
trianglePlot(~copper+lead+zinc, data=lat.lon.meuse)   

#  ... with Cd as zcase 
#  (bubble plot, col and cex scaling)
trianglePlot(cadmium~copper+lead+zinc, data=lat.lon.meuse)

#  ... and conditioning by lime
trianglePlot(cadmium~copper+lead+zinc|lime, data=lat.lon.meuse)


##  Example 2
##  Local scales/axis control in trianglePlot

#  tri.frame - a 'standalone' function to demonstrate the
#  axes, ticks, grids, annotation handling in trianglePlot

tri.frame <- function(panel=panel.triangleGrids, axes = TRUE, ticks = TRUE, 
                      grids = TRUE, annotation = TRUE, ...){

    #default plot
    #blank canvas
    my.plot <- list(x=1~1, xlim=c(-0.2,1.1), ylim=c(-0.2,1.1), type = "n", 
                    xlab = "", ylab ="", panel = panel)

    #what you add
    extra.args <- list(...)

    #made local scale for a, b c axes
    #with localScalesHandler
    temp <- listUpdate(extra.args, list(allowed.scales = c("a", "b", "c"), 
                                        disallowed.scales = c("x", "y"), 
                                        remove.box = TRUE))
    extra.args <- listUpdate(extra.args, do.call(localScalesHandler, temp), 
                             ignore.a = "scales")

    #get sensible setting for axes, grids, ticks, annotation
    extra.args$axes <- getPlotArgs("axis.line", user.resets=axes, 
                                   elements = c("a", "b", "c"))
    extra.args$grids <- getPlotArgs("axis.line", user.resets=grids, 
                                    elements = c("a", "b", "c"))
    extra.args$ticks <- getPlotArgs("axis.line", user.resets=ticks, 
                                    elements = c("a", "b", "c"))
    extra.args$annotation <- getPlotArgs("axis.text", user.resets=annotation, 
                                    elements = c("a", "b", "c"))

    #update my.plot
    my.plot <- listUpdate(my.plot, extra.args)

    do.call(xyplot, my.plot)

}

#  You can send this any of the above panels
#  (works like x,y scales in lattice but for 
#  a,b,c scales of trianglePlot) 

tri.frame(panel.triangleGrids) #just the triangle plot grids
tri.frame(panel.triangleAxes)  #just the triangle plot axes
tri.frame(panel.triangleFrame)  #triangle plot 'frame' of both

tri.frame(panel.triangleAxes, axes = 1:3) #modify appearances
tri.frame(panel.triangleGrids, grids = list(a = list(lty=1, col = "green"),
                                            b = list(lty=2, col = "blue"),
                                            c = list(lty=3, col = "purple")))

tri.frame(panel.triangleFrame, scales = list(tick.number = 2)) #modify scales



}


\keyword{ methods }

\name{1.1.loaPlot}
\alias{1.1.loaPlot}
\alias{loaPlot}
\alias{panel.loaPlot}
\alias{panel.loaPlot2}
\alias{panel.loaGrid}


\title{
  loaPlot, XYZ plots for lattice
}

\description{
  loaPlot is a standard XYZ plotting function, where X and Y are the axes of 
  a conventional XY plot and Z is an element (or elements if supplied in the 
  form Z1 + Z2 + Z3...) visualized at associated XY coordinates. By default, 
  loaPlot links Z to plot point size and color to generate a bubbleplot style 
  output, or using modified plot calls other plot types.   
}

\usage{
  loaPlot(x, data = NULL, panel = panel.loaPlot, 
        ..., local.scales = FALSE, reset.xylims = TRUE, 
        load.lists = NULL, by.group = NULL, by.zcase = NULL, 
        preprocess = TRUE)

  #standard panels

  panel.loaPlot(..., loa.settings = FALSE)
  panel.loaPlot2(..., loa.settings = FALSE)

  #grids

  panel.loaGrid(grid.x = NULL, grid.y = NULL,
        xlim = NULL, ylim = NULL, ..., 
        grid = NULL, panel.scales = NULL)

}

\arguments{

  \item{x}{ A formula with the general structure 
   \code{z ~ x * y | cond} applied like in the \code{\link{lattice}} 
   function \code{\link{levelplot}} or a matrix. For a formula, \code{x} 
   and \code{y} are the horizontal and vertical axes, \code{z} is any 
   additional information to be used in point, symbol, surface or glyph 
   generation, and \code{cond} is any additional conditioning to be applied. 
   \code{x} and \code{y} are required elements; \code{z} and \code{cond} are 
   typically optional.\cr
   (Note: this element of the plot is handled by \code{\link{formulaHandler}}).
   }

  \item{data}{ If supplied, the assumed source of elements of \code{x}, 
   typically a \code{data.frame}.
   }

  \item{panel}{ \code{panel} is the function to be used when generating 
   the content of the individual panels within the \code{\link{lattice}} 
   plot. By default, this is the \code{loa} panel function \code{panel.loaPlot}. 
   }

  \item{...}{ Additional arguments are passed on to related functions. 

   For \code{loaPlot} these are \code{\link{colHandler}}, 
   \code{\link{cexHandler}} and the function set by \code{panel}.
   
   This mechanism provides access to most common plot parameters, e.g. \code{col}, 
   \code{pch}, and \code{cex} for plot symbol color, type and size, respectively.
   By default, both data point color and size are \code{z}-scaled for \code{loaPlot}. 
   If \code{z} is supplied, and \code{cex} and \code{col} are not set by the user in the 
   plot command, these plot properties are managed by \code{\link{cexHandler}} and 
   \code{\link{colHandler}}, respectively. \code{\link{cexHandler}} and 
   \code{\link{colHandler}} arguments can be also be passed directly as part of the 
   \code{loaPlot} command to fine-tune these, e.g. \code{cex.range} to change the \code{cex} 
   range that \code{z} values are scaled to and \code{col.region} to change the color range 
   that is applied to \code{z} when coloring points. See associated Help documents for further 
   information.
   }

   \item{local.scales}{For \code{loaPlot} only, logical. If \code{TRUE}, this removes the 
   standard \code{lattice} axis from the plot. It is intended to be used with \code{panel} 
   functions which generate their own axes or have no axes.
   }

   \item{reset.xylims}{For \code{loaPlot} only, logical or character vector. If a logical, if 
   the panel outputs are preprocessed (using \code{panelPal}), should the x and y limits be 
   reset? If a character vector, one or more terms controlling post-processing plot range 
   management: \code{refit.xylims}, equivalent to \code{reset.xylims = TRUE}; and \code{max.xylims}, 
   to reset both x and y ranges to maximum.\cr
   (Note: If \code{xlim} or \code{ylim} are supplied in the plot call, these will typically 
   override all \code{reset.xylims} settings.) 
   }

   \item{load.lists}{For \code{loaPlot} only, character vector. In-development alternative to 
   list based arguments. This option identifies plot call arguments that \code{loaPlot} should 
   manage using \code{\link{listLoad}}. See associated help documentation for further details.   
   }

   \item{by.group, by.zcase}{For \code{loaPlot} only. Arguments for routine 
   by group and by zcase handling of plot inputs.\cr 
   Important: These are current under review.
   }
 
   \item{preprocess}{For \code{loaPlot} only, logical, passed to \code{\link{panelPal}}. If 
   \code{TRUE}, and used with a correctly configured \code{panel} function, this processes 
   the plot input before generating the plot. This means color scales in the different plot 
   panels and the key are automatically aligned and the associated trellis object output 
   contains the \code{panel} function outputs rather than the inputs. See \code{\link{panelPal}} 
   Help documents for further information.
   }

   \item{loa.settings}{For \code{panel...} functions only, logical, passed to \code{\link{panelPal}} 
   to manage plot reworking. See associated Help documents for further information.
   } 

   \item{grid.x, grid.y, xlim, ylim, grid, panel.scales}{For \code{panel.loaGrid} only, grid 
   settings, typically recovered by \code{loaPlot}. 
   }

}

\details{

  \code{loaPlot} provides lattice-style conditioning/handling for a range of 
   commonly used XYZ plotting options. It is perhaps easiest pictured as a 
   'mid point' alternative somewhere between the standard \code{\link{lattice}} 
   plot functions \code{\link{xyplot}} and \code{\link{levelplot}}.

   The default form of the plot uses an extension of the subscripting methods 
   described by Deepayan Sarkar in Chapter 5 of Lattice (see sections on 
   scatterplots and extensions). The default output is a bubble plot (see example 1 
   below). 
    
}

\value{

  \code{loaPlot} returns a trellis object, much like a conventional \code{lattice} 
   plot function.
   
}

\note{

  \code{panel.loaPlot2} is an alternative versions of \code{panel.loaPlot} that is 
  currently under revision. Please use with care.

  \code{loaPlot} arguments \code{by.group} and \code{by.zcase} are currently in 
  revision. Please use with care. 
     
}


\references{
  These functions make extensive use of code developed by others.  

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

   In \code{loa}: \code{\link{panelPal}} 

   In other packages, see

   \code{\link{lattice}}: \code{\link{xyplot}}; and \code{\link{levelplot}}.

}

\examples{

## Example 1
## Basic usage

loaPlot(Ozone~Solar.R*Temp|Wind>8, 
        data=airquality, 
        col.regions="Blues")

# Notes:
# Formula structure z ~ x * y |cond like levelplot.
# Data (and groups) assignment like in standard lattice plots.
# By default z is linked to col and cex. 
# Unless overridden by user inputs or group or zcase setting. 
# Plot passed via ...Handler functions to provide shortcut plot 
# reworking, e.g. here colHandler handles color scales 
# using col.region to generate a color range. 
# (Here, arguments like "Blues" and c("green", "red") are 
# allowed and handled using functions in the RColorBrewer 
# package.)

# Formula stuctures:
# ~ x * y                like xyplot y ~ x
# ~ x * y | cond         like xyplot y ~ x | cond
# z ~ x * y              like xyplot y ~ x, col=f(z), cex=f(z)
# z ~ x * y | cond       like xyplot y ~ x | cond, col=f(z), cex=f(z)
# z ~ x * y, groups = g  like xyplot y ~ x, groups=g, cex=f(z)
# z1 + z2 ~ x * y        (zcases)
# etc


## Example 2 
## Basic modifications

loaPlot(Ozone~Solar.R*Temp, groups=airquality$Wind>8, 
        data=airquality) 

# When groups are applied, by default group id is linked to col.
# The follow example illustrates three options:

loaPlot(Ozone~Solar.R*Temp, groups=airquality$Wind>8, 
        data=airquality,  
        group.args=c("pch"), pch=c(1,4), 
        col="blue")

# notes:
# Here, group.args is used to change the default group arguments.
# (So, pch rather than col is used to identify groups.)
# pch is then assigned by group rather than by (x,y) case or z case. 
# (See panelPal Help further further details of assignments in loa.)
# col supplied by the user supercedes the default z linkage.
# (So, here cex remains z scales but col is fixed as blue.)


## Example 3 
## Key handling

loaPlot(Ozone~Solar.R*Temp, data=airquality, 
        col.regions=c("green", "red"))

# Key settings are by the key argument (as in lattice) 
# or key... arguments via keyHandler and listLoad, so e.g.:

loaPlot(Ozone~Solar.R*Temp, data=airquality, 
        col.regions=c("green", "red"), 
        key.fun = draw.loaColorKey)

# Notes:
# By default the loaPlot uses draw.loaPlotZKey to generate 
# its color key unless an alternative is supplied via key.fun.
# (Here, the draw.colorKey wrapper draw.loaColorKey is used to 
# generate a color bar similar to that in levelplot.)



## Example 4
## panels

loaPlot(Ozone~Solar.R*Temp|Wind>8, data=airquality, 
        col.regions="Reds")

# The combined use of loaPlot, panelPal and appropriately configured 
# panel functions provides automatical handling of a range of plot 
# elements, e.g.:

loaPlot(Ozone~Solar.R*Temp|Wind>8, data=airquality, 
        col.regions="Reds", panel=panel.binPlot)

# Notes:
# Here, the choice of default key is set by the panel... function;
# the panel by default bins data by location and for each bin cell
# calculates the mean Ozone concentration just like a standard 
# lattice panel would, but it also tracks these values (calculated 
# within the panels) and scales panel-to-panel and panel-to-key 
# so users do not have to do that retrospectively; and, finally, 
# it retains in-panel calculations so users can recover them.
# (See associated helps for further details: ?panelPal about methods; 
# and ?panel.binPlot about the panel function.)
 
}

\keyword{ methods }

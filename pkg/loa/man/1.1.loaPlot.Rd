\name{1.1.loaPlot}
\alias{1.1.loaPlot}
\alias{loaPlot}
\alias{formulaHandler}


\title{
  loaPlot, XYZ plots for lattice
}
\description{
  XYZ plotting options including bubbleplots.
}
\usage{

  loaPlot(x, data = NULL, panel = panel.xyplot, 
        ..., safe.mode = TRUE, preprocess = TRUE, 
        reset.xylims = TRUE)

  formulaHandler(x, data = NULL, ..., 
        check.xy.dimensions = TRUE)

}

\arguments{

  \item{x}{ A formula with the general structure 
   \code{z ~ x * y | cond} applied like in the \code{\link{lattice}} 
   function \code{\link{levelplot}}. \code{X} and \code{y} are the 
   horizontal and vertical axes, \code{z} is any additional information 
   to be used in point, symbol, surface or glyph generation, and 
   \code{cond} any additional conditioning to be applied. (\code{X} and 
   \code{y} are required elements; \code{z} and \code{cond} are typically 
   optional.)
   }

  \item{data}{ If supplied, the assumed source of the elements of formula 
   \code{x}, typically a \code{data.frame}.
   }

  \item{panel}{ \code{panel} is the function to be used when generating 
   the content of the individual panels within the \code{\link{lattice}} 
   plot. By default, this is the standard \code{lattice} scatter plot panel 
   \code{\link{panel.xyplot}}. 
   }

  \item{...}{ Additional arguments are passed on to related functions. 

   For \code{loaPlot} these are \code{\link{cexHandler}}, 
   \code{\link{cexHandler}} and \code{\link{xyplot}}.
   
   This mechanism provides access to most common plot parameters, e.g. \code{col}, 
   \code{pch}, and \code{cex} for plot symbol color, type and size, respectively.

   Note: By default, both data point colour and size are \code{z}-scaled for \code{loaPlot}. 
   If \code{z} is supplied, and \code{cex} and \code{col} are not set by the user in the 
   plot command, these plot properties are managed by \code{\link{cexHandler}} and 
   \code{\link{colHandler}}, respectively. \code{\link{cexHandler}} and 
   \code{\link{colHandler}} arguments can be also be passed directly as part of the 
   \code{loaPlot} command to fine-tune these, e.g. \code{cex.range} to change the \code{cex} 
   range that \code{z} values are scaled to and \code{col.region} to change the color range 
   that is applied to \code{z} when coloring points. See associated Help documents for further 
   information.
   }

   \item{safe.mode, preprocess, reset.xylims}{For \code{loaPlot} only, all logicals, and all 
   passed to \code{\link{panelPal2}} to manage plot reworking. See associated Help documents 
   for further information.
   } 

   \item{check.xy.dimensions}{For \code{formulaHandler} only, logical. Should the formula 
   structure be tested before attempting to generate the plot?  
   }

}

\details{
  NOTE: \code{loaPlot} is currently in development and may be subject to changes.

  \code{loaPlot} provides lattice-style conditioning/handling for a range of 
   commonly used XYZ plotting options.
 
}

\value{

  \code{loaPlot} returns a trellis object, much like a conventional \code{lattice} 
   plot function.
   
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

   \code{\link{panelPal2}}

   For information of related functions in other packages, see

   \code{\link{lattice}}: \code{\link{xyplot}}; \code{\link{panel.xyplot}}; and 
   \code{\link{panel.levelplot}}.

}

\examples{
#TO BE CONFIRMED


}

\keyword{ methods }

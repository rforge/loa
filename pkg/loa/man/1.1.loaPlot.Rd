\name{1.1.loaPlot}
\alias{1.1.loaPlot}
\alias{loaPlot}
\alias{panel.loaPlot}
\alias{panel.loaPlot2}




\title{
  loaPlot, XYZ plots for lattice
}
\description{
  XYZ plotting options including bubbleplots.
}
\usage{

  loaPlot(x, data = NULL, panel = panel.loaPlot, 
        ..., local.scales = FALSE, reset.xylims = TRUE, 
        load.lists = NULL, by.group = NULL, by.zcase = NULL, 
        preprocess = TRUE)

  #standard panels

  panel.loaPlot(..., loa.settings = FALSE)

  panel.loaPlot2(..., loa.settings = FALSE)

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
   plot. By default, this is the \code{loa} panel function \code{panel.loaPlot}. 
   }

  \item{...}{ Additional arguments are passed on to related functions. 

   For \code{loaPlot} these are \code{\link{colHandler}}, 
   \code{\link{cexHandler}} and the function set by \code{panel}.
   
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

   \item{local.scales}{For \code{loaPlot} only, logical. If \code{TRUE}, this removes the 
   standard \code{lattice} axis from the plot. It is intended to be used with \code{panel} 
   functions which generate their own axis have no axis.
   [EXAMPLES TO DOCUMENT]
   }

   \item{reset.xylims}{For \code{loaPlot} only, logical. If the panel outputs are 
   preprocessed, should the x and y limits be reset (if not given in plot called by \code{xlim} 
   or \code{ylim}).
   }

   \item{load.lists}{For \code{loaPlot} only. Under development alternative to list based 
   arguments. [TO BE DOCUMENTED]. 
   }

   \item{by.group, by.zcase, by.cond}{For \code{loaPlot} only. Under development arguments for routine 
   by group and by zcase handling of plot inputs. [TO BE DOCUMENTED].
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

   \code{\link{panelPal}}

   For information on related functions in other packages, see

   \code{\link{lattice}}: \code{\link{xyplot}}; \code{\link{panel.xyplot}}; and 
   \code{\link{panel.levelplot}}.

}

\examples{
#TO BE CONFIRMED


}

\keyword{ methods }

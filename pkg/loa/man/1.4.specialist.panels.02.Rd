\name{1.4.specialist.panels.02}
\alias{1.4.specialist.panels.02}
\alias{panel.polarPlot}
\alias{panel.polarFrame}
\alias{panel.polarAxes}
\alias{panel.polarGrids}
\alias{panel.polarLabels}


\title{
  Special panel functions 01
}
\description{
  In development specialist panel functions for polar plotting
}
\usage{

panel.polarPlot(x = NULL, y = NULL, r = NULL, theta = NULL, ..., 
         data.panel = panel.loaPlot, loa.settings = FALSE, 
         plot = TRUE, process = TRUE)


#grid, axes and axes labelling

panel.polarFrame(..., grids = TRUE, axes = TRUE, labels = TRUE, 
         panel.scales = NULL, grids.panel = panel.polarGrids, 
         axes.panel = panel.polarAxes, labels.panel = panel.polarLabels)

panel.polarAxes(axes.theta = NULL, axes.r = NULL, thetalim = NULL, 
         rlim = NULL, ..., axes = NULL, panel.scales = NULL)

panel.polarGrids(grids.theta = NULL, grids.r = NULL,
         thetalim = NULL, rlim = NULL, ..., grids = NULL, 
         panel.scales = NULL)

panel.polarLabels(labels.theta = NULL, labels.r = NULL,
         thetalim = NULL, rlim = NULL, ..., labels = NULL, 
         panel.scales = NULL)


}

\arguments{

  \item{x, y}{ The \code{x} and \code{y} coordinates of plot points. 
   }

  \item{r, theta}{ The equivalent polar coordinates of the plot 
   points. If these are not supplied, \code{x} and \code{y} are 
   assumed to be polar coordinates and these are calculated 
   by the function. 
   }


  \item{...}{ Additional arguments, typically passed on. For 
  \code{panel.polarPlot} these are passed to the \code{data.panel}. 
  See below.  
  } 

 \item{data.panel}{ The panel to use to handle data once polar 
  coordinates have been checked for or generated. For 
  \code{panel.polarPlot}, by default this is \code{panel.loaPlot}.  
  } 

 \item{loa.settings, plot, process}{ \code{loa} panel management 
  arguments, handled by \code{\link{panelPal}}. See associated 
  HELP documentation for further details. 
  } 

 \item{grids, axes, labels}{ plot management options for the grid, axis 
   and axis label elements of the plot. These can be logicals (\code{TRUE} 
   to include the element or \code{FALSE} to remove it) or lists of plot 
   parameters.
  }
 
 \item{panel.scales}{ \code{loa} plot management argument used when 
  generating grids, axes and labels within the plot panel.
  }

 \item{grids.panel, axes.panel, labels.panel}{ Used by the \code{panel...Frame} 
  functions to identify the \code{panel...} functions to use when 
  generating the grid, axes and axis labelling elements of the plot.
  }

 \item{axes.theta, axes.r, thetalim, rlim}{ For \code{panel.polarAxes} 
  axes settings. \code{axes.theta} and \code{axes.r} are the \code{theta} and 
  \code{r} coordinates of the axis reference points, tick marks, etc. 
  \code{thetalim} and \code{rlim} are the plot/axes ranges (like \code{xlim} and 
  \code{ylim} in standard \code{lattice} plots).
  }

\item{grids.theta, grids.r}{ Like \code{axes.theta} and \code{axes.r} but for 
  grids.
  } 

\item{labels.theta, labels.r}{ Like \code{axes.theta} and \code{axes.r} but for 
  labels.
  } 

} 


\details{
  The \code{panel.polar...} series of the functions are intended for 
  use with \code{\link{loaPlot}}. 

  \code{panel.polarPlot} generates a 'bubble plot' style output on 
  polar coordinates. It generates axes and annonation within each 
  plot panel using the other panel functions.

  \code{panel.polarGrids}, \code{panel.polarAxes} and \code{panel.polarLabels} 
  generate plot grid, axes and axes labelling elements of the plot. 
  \code{panel.polarFrame} provides a wrapper for these plot elements. 

  Users can fine-tune axes, grids and labels by supplying additional arguments 
  in plot calls, or replace these elements with purpose written functions 
  to more completely modify plot appearance.

}

\value{

  The \code{panel.polar...} functions are intended to be used as the 
  \code{panel} argument in \code{loa} plot calls. So, e.g.:

  \code{a <- 1:360}
  
  \code{loaPlot(a~a*a, panel=panel.polarPlot)}

  They can also be used with relatively simple \code{lattice} plots. 
  However, some features of \code{loa} plots managed by 
  \code{\link{panelPal}}, e.g. default plot appearance management, 
  automatic grouping and panel and key alignment will not be available.     
   
}

\references{
  These function makes extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5
 

}

\author{
  Karl Ropkins
}

\note{

  \code{panel.polarPlot} is IN-DEVELOPMENT. Please handle with care. 

}

\seealso{

   \code{\link{loaPlot}}, \code{\link{panelPal}}

   \code{\link{lattice}}, \code{\link{xyplot}},
}

\examples{

##TO DO

#etc.

}


\keyword{ methods }

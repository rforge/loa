\name{2.3.specialist.panels}
\alias{2.3.specialist.panels}
\alias{panel.zcasePiePlot}
\alias{panel.zcasePieSegmentPlot}


\title{
  Special panel functions 03
}
\description{
  In development specialist panel functions for generating zcase glyph structures.
}
\usage{

panel.zcasePiePlot(..., loa.settings = FALSE)

panel.zcasePieSegmentPlot(..., zcase.rescale=TRUE, 
         loa.settings = FALSE)

}

\arguments{


  \item{...}{ Additional arguments, typically setting the color and 
   properties of the plotted glyphs. See below.  
  } 

  \item{zcase.rescale}{ Should the glyph element be rescaled? See below.}

  \item{loa.settings}{ loa options, to be handled by \code{panelPal}. 
   }

} 


\details{

  All these \code{panel...} functions generate glyphs using \code{z} inputs and plot these 
  at the associated (x, y) location. So, for example a called which used one of the panels 
  and the plot formula: 

  \code{z1 + z2+ z3 + z4 ~ x * y} 

  ... would plot a series of glyphs, each containing four elements that would be scaled 
  according to \code{z1}, \code{z2}, \code{z3} and \code{z4}, and each at the associated 
  (x, y) location. This means there will be one discrete glyph for each row of data supplied 
  to the plot call.  
 
  \code{panel.zcasePiePlot} generates a series of x/y referenced pie graphs. By default, pie 
  dimensions are assigned as: Pie size (radius) proportional to sum of z cases and scaled using 
  \code{\link{cexHandler}} (z1 + z2 + z3 + z4 for the above formula); Number of Pie segments 
  equal to number of z cases (so, 4 for the above formula); Pie segment width (phi) proportional to 
  the individual zcase (so, z1 / (z1 + z2 + z3 + z4)  * 360 for first pie segment for the above 
  formula). 

  \code{panel.zcasePieSegmentPlot} is a variation on the conventional pie plot where segement radius 
  rather than segment width is varying by zcase.    

}

\value{

  These \code{panel...} functions are intended to be used as the 
  \code{panel} argument in \code{loa} plot calls. So, e.g.:

  \code{a <- 1:10}\cr
  \code{b <- 10:1}\cr
  \code{loaPlot(a+b~a*a, panel=panel.zcasePiePlot)}\cr     
  \code{loaPlot(a+b~a*a, panel=panel.zcasePieSegmentPlot)}\cr
   
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

  Functions in development. Arguments may change, e.g.: 

  \code{panel.zcasePieSegmentPlot} includes the argument \code{zcase.rescale}. 
  This normalises data within each zcase before generating the pie segments. This 
  might not stay. 
  
}

\seealso{

   In \code{loa}: \code{\link{loaPlot}}, \code{\link{panelPal}}

   In other packages: \code{\link{xyplot}} in \code{\link{lattice}}.
}

\examples{

## Example 1
## plotting georeferenced pie plots

#  Using a subsample of lat.lon.meuse
temp <- lat.lon.meuse[sample(1:155, 15),]

\dontrun{
#  plot Cu/Pb/Zn pie plots at sampling locations
   loaPlot(copper+lead+zinc~longitude*latitude, 
           panel=panel.zcasePiePlot, data=temp)
#  then rescale smaller pie segments on the fly}

loaPlot(copper*10+lead*4+zinc~longitude*latitude, 
        panel=panel.zcasePiePlot, data=temp)

}


\keyword{ methods }

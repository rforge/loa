\name{2.3.specialist.panels}
\alias{2.3.specialist.panels}
\alias{panel.zcasePieSegmentPlot}


\title{
  Special panel functions 03
}
\description{
  In development specialist panel functions for generating zcase glyph structures
}
\usage{

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

  All these \code{panel...} functions generate glyphs using \code{z} inputs. 

  The \code{panel.zcasePieSegmentPlot} generates... 

}

\value{

  These \code{panel...} functions are intended to be used as the 
  \code{panel} argument in \code{loa} plot calls. So, e.g.:

  \code{a <- 1:10}\cr
  \code{b <- a*10}\cr
  \code{loaPlot(a+b~a*a, panel=panel.zcasePieSegmentPlot)}     
   
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

  IN-DEVELOPMENT. 

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

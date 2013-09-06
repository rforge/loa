\name{2.4.specialist.panels}
\alias{2.4.specialist.panels}
\alias{panel.compareZcases}


\title{
  Special panel functions 03
}
\description{
  In development specialist panel functions 
}
\usage{

panel.compareZcases(x=x, y=y, z=NULL, ..., 
         loa.settings = FALSE) 

}

\arguments{

  \item{x, y, z}{ Standard plot data series, typically vectors}

  \item{...}{ Additional arguments. See below.  
  } 

  \item{loa.settings}{ loa options, to be handled by \code{panelPal}. 
   }

} 


\details{ 

  The \code{panel.compareZcases} generates...
 
}

\value{

  These \code{panel...} functions are intended to be used as the 
  \code{panel} argument in \code{loa} plot calls. So, e.g.:

  \code{a <- 1:10}\cr
  \code{b <- a}\cr
  \code{c <- a}\cr
  \code{c[5:6] <- c(1,10)}\cr
  \code{loaPlot(c~a*b, panel=panel.compareZcases, col.regions="Reds")}     
   
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

  These functions are IN-DEVELOPMENT. 

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

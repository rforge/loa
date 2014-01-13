\name{2.4.specialist.panels}
\alias{2.4.specialist.panels}
\alias{panel.compareZcases}


\title{
  Special panel functions 04
}
\description{
  In development specialist panel functions 
}
\usage{

panel.compareZcases(x=x, y=y, z=NULL, ..., 
         loa.settings = FALSE) 

}

\arguments{

  \item{x, y, z}{ Standard plot data series, typically vectors.
  }

  \item{...}{ Additional arguments, typically passed on.  
  } 

  \item{loa.settings}{ loa options, to be handled by \code{panelPal}. 
   }

} 


\details{ 

  The \code{panel.compareZcases} generates a simple plot which 
  compares \code{z} and \code{y} elements.
 
}

\value{

  These \code{panel...} functions are intended to be used as the 
  \code{panel} argument in \code{loa} plot calls. So, e.g.:

  \code{x <- 1:10}\cr
  \code{y <- 1:10}\cr
  \code{z <- y + rnorm(10)}\cr
  \code{loaPlot(z~x*y, panel=panel.compareZcases, col.regions="Reds")}\cr

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

  These are ad hoc \code{panel...} functions. Not sure of their 
  life expectancy... 

}

\seealso{

   In \code{loa}: \code{\link{loaPlot}}, \code{\link{panelPal}}.

   In other packages: \code{\link{xyplot}} in \code{\link{lattice}}.
}

%examples to do
%\examples{
%##TO DO
%#etc.
%}


\keyword{ methods }

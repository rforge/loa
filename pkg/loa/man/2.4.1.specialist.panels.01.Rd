\name{2.4.1.specialist.panels}
\alias{2.4.specialist.panels}
\alias{panel.kernelDensity}

\title{
  Special panel functions 01
}
\description{
  In development specialist panel functions for use with lattice
}
\usage{

panel.kernelDensity(x, y, z = NULL, ..., n = 20, 
         kernel.fun = NULL, panel.range = TRUE) 
}

\arguments{

  \item{x, y, z}{ \code{lattice} function arguments passed down to 
   individual panels. 
   }

  \item{...}{ Additional arguments, typically passed on. See below.
  } 

 \item{n}{ For \code{panel.kernelDensity}, the number of x and y 
  cases to estimate when estimating density.  
  } 
 \item{kernel.fun}{ A function that can estimate kernel densities. 
  } 
 \item{panel.range}{ For \code{panel.kernelDensity}, a logical (default 
  \code{FALSE}) indicating if the kernel density estimation data range 
   should be forced to the full panel range. See Below   
  }


} 


\details{
  Special \code{panel...} functions generate provide alternative 
   data handling for \code{lattice} functions like \code{xyplot}. 
   They are intended to be used in the conventional form, e.g.:

   \code{xyplot(..., panel = panel.kernelDensity)}

   \code{xyplot(..., panel = function(...) panel.kernelDensity(..., n = 50))}
   
   \code{#etc}
  

  \code{panel.kernelDensity} generates kernel density estimations 
   based on the supplied \code{x} and \code{y} data ranges.

}

\value{

  As with other \code{panel...} functions, output suitable for 
  use as the \code{panel} argument in \code{lattice} plot calls. 
   
}

\references{
  These function makes extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5
 
  (for \code{panel.kernelDensity}) MASS package:
  Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics 
  with S. Fourth edition. Springer. 

}

\author{
  Karl Ropkins
}

\note{

  \code{panel.kernelDensity} passes additional arguments on to the 
   \code{kernel.fun} to estimate kerenel density and the \code{lattice} 
   function \code{panel.contourplot} to generate the associated plot. 
   If no \code{kernel.fun} is supplied in the \code{panel} call, the 
   \code{MASS} function \code{kde2d} is used to estimate kernel density.  

   \code{}


}

\seealso{
   \code{\link{lattice}}, \code{\link{xyplot}}, \code{\link{panel.contourplot}},
}

\examples{

a <- rnorm(1000)
b <- rnorm(1000)

xyplot(a~b, panel = panel.kernelDensity)

#etc.

}


\keyword{ methods }

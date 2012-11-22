\name{2.4.panel.pal}
\alias{2.4.panel.pal}
\alias{panelPal}
\alias{panelPal2}

\title{
  panelPal
}
\description{
  \code{lattice} plot management using the \code{loa} function \code{panelPal}
}
\usage{

panelPal(x, y, subscripts, at, col.regions, ..., 
         panel = panel.xyplot, ignore = NULL, 
         group.fun = NULL)

panelPal2(ans, panel = NULL, preprocess = FALSE, 
         safe.mode = TRUE, reset.xylims = FALSE, 
         group.fun = NULL)

}

\arguments{

  \item{x,y,subscripts,at,col.regions}{ \code{lattice} 
   function \code{xyplot} arguments passed down to individual 
   panels. 
   }

  \item{...}{ Additional arguments, typically passed on.
  } 

 \item{panel}{ A panel function that the above are all passed 
  on to, e.g. \code{panel.xyplot}.  
  } 
\item{ignore}{ Any additional arguments that \code{panelPal} 
  should ignore and pass on to \code{panel} unchecked/unmodified. 
  DETAILS TO BE CONFIRMED? 
  } 

 \item{group.fun}{ [IN DEVELOPMENT - PLEASE HANDLE WITH CARE]. 
  \code{group.fun} provides fine control of the standard \code{lattice} 
  plot argument \code{group}. It can be a vector or list containing the 
  same number of elements as there as groups. These can be functions (or 
  the names of functions as characters) setting individual functions for 
  group or sets of parameters to be evaluated using the \code{panel} 
  function. For example, the current \code{NULL} default generates a list 
  of \code{col} and \code{pch} settings that produce a conventional grouped 
  scatter plot output when the \code{group} argument is applied to the 
  \code{panel} default \code{panel.xyplot}. See Details below. 
  }

 \item{ans, preprocess, safe.mode, reset.xylims}{ [OPTIONS FOR IN DEVELOPMENT 
  PANELPAL ALTERNATIVE - PLEASE HANDLE WITH CARE]. \code{ans} - The 
  standard \code{trellis} object to be modified. \code{proprocess} - Logical, 
  should the supplied \code{panel} function be preprocessed? \code{safe.mode} - 
  Logical, should the plot be generated using safe.mode? \code{reset.xylims} - 
  Logical, should the plot dimensions be reset if changed, e.g. by preprocessing?  
  See Details below. 
  }

} 


\details{
  \code{panelPal} provides an alternative mechanism to 
   \code{panel.superpose} and \code{panel.groups} for work with 
   the \code{group} argument and the conditioning/handling of 
   non-standard plot arguments. The \code{group.fun} argument 
   allows users to more easily apply different group coding, vary 
   coding options by group or even apply completely different 
   \code{panel...} functions to each group. This means 
   that different data groups can be more easily visualised in 
   completely different ways within the same panel. 

  \code{panelPal2} is an in-development alternative to the current 
   \code{panelPal}. If used with appropriately structured 
   \code{panel...} functions, this allows in-panel code to be 
   preprocessed so panels and legend settings can be aligned. 

}

\value{
  \code{panelPal} is intended for use with \code{xyplot}, as a 
   wrapper for standard \code{panel...} functions.

  \code{panelPal2} is an alternative to \code{panelPal}, that 
   can be used to modify a previously generated \code{trellis} 
   plot output.

}

\references{
  These function makes extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

  (\code{panelPal2} applies an extension of the hexbin plot method 
  briefly discussed in Chapter 14 of Sarkar.)

}

\author{
  Karl Ropkins
}

\note{
 To be confirmed...

}

\seealso{
   \code{\link{lattice}}, \code{\link{xyplot}},
}

\examples{

#to be confirmed

}


\keyword{ methods }

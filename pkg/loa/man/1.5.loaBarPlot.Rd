\name{1.5.loaBarPlot}
\alias{1.5.loaBarPlot}
\alias{loaBarPlot}


\title{
  loaBarPlot
}

\description{
  Bar plot variation using for Student Project. 
}

\usage{
   loaBarPlot(x, y=NULL, groups=NULL, cond=NULL, 
            data=NULL, ..., drop.nas=TRUE, stat=NULL)  
}

\arguments{

  \item{x}{ Either the x case for the bar plot or a plot formula. If the x case, 
   typically a vector of factor or grouping terms, used to assign x positions in 
   bar plot. If a plot formula, a plot description in the format y~x|cond, where 
   x is a factor or grouping term and y and cond are optional.}

  \item{y}{ (Optional) The y case for the bar plot, typically a vector of numeric 
   terms, used with \code{stat} when calculating summary information for bar plots.}

  \item{groups, cond}{ (Optional) The group case for the bar plot, typically a vector 
   of factor or grouping terms.}

  \item{data}{ (Optional) if supplied, the assumed source 
   of the plot elements, \code{x}, \code{y}, \code{groups} and \code{cond}, 
   typically a \code{data.frame}.
   }

  \item{...}{ Additional arguments, passed on to \code{lattice} function.
   }

  \item{drop.nas}{ Option to drop \code{NAs} before plotting results, default 
   \code{TRUE}.
   }

  \item{stat}{ If supplied, the function used to summarise \code{y} data after grouping 
   by \code{x}, \code{groups} and \code{cond}. By default, this counts \code{x} cases 
   if \code{y} is not supplied, or calculates sum of \code{y} values if these are 
   supplied.
   }  
  
}


\details{

   \code{loaBarPlot} summarises supplied plot data and generates a bar plot using the 
    lattice framework.

} 


\value{

   \code{loaBarPlot} returns trellis objects, much like 
   conventional \code{lattice} plot functions.

}

\references{

  These function makes extensive use of code developed by others.  

  As elsewhere, the use of \code{lattice} is also gratefully acknowledged:

  lattice:
  Sarkar, Deepayan (2008). Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5
}

\author{
  Karl Ropkins
}


\note{

   Development:

   This is an in-development plot, and 'best handling' strategies have not 
   been decided for several elements. So, future versions of these functions 
   may differ significantly from the current version. 

   In particular:

   \code{loaBarPlot}:

   This is for student project, may not be staying.

   Code currently in revision. Please handle with care.
   
              
}

\seealso{

   In \code{loa}: \code{\link{listUpdate}} and \code{\link{colHandler}}. 

   In other packages: \code{\link{barchart}} in \code{\link{lattice}}. 

}

\examples{

## Example 1
## Basic bar plot usage

loaBarPlot(Species, Sepal.Width, data=iris, stat=mean)

#or equivalent using formula 
## loaBarPlot(Sepal.Width~Species, data=iris, stat=mean)
 
}


\keyword{ methods }

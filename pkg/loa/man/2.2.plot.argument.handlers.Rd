\name{2.2.plot.argument.handlers}
\alias{2.2.plot.argument.handlers}
\alias{cexHandler}
\alias{colHandler}
\alias{pchHandler}
\alias{zHandler}


\title{
  Common plot argument handlers
}
\description{
  Functions for use the routine handling of some common plot arguments.
}

\usage{

cexHandler(z = NULL, cex = NULL, 
           cex.range = NULL, expand.outputs = TRUE, 
           ref = NULL, ..., zlim = NULL)

colHandler(z = NULL, col = NULL, 
           region = NULL, colorkey = FALSE, legend = NULL,
           pretty = FALSE, at = NULL, cuts = 20,
           col.regions = NULL, alpha.regions = NULL,
           expand.outputs = TRUE, ref = NULL, 
           ..., zlim = NULL, output="col")

pchHandler(z = NULL, pch = NULL, pch.order = NULL, 
           expand.outputs = TRUE, ref = NULL, ..., 
           zlim = NULL)

zHandler(z = NULL, expand.outputs = TRUE, 
           ref = NULL, ...)

}

\arguments{

  \item{z}{If supplied, a vector of values intended to used as a scale 
   when assigning a property.

   For \code{cexHandler}, the cex of, e.g., points on a 
   scatter plot. Here, size scales are managed using a reference range 
   \code{cex.range}, but superseded by \code{cex} settings, if also 
   supplied.

   For \code{colHandler}, the color of, e.g., points on a 
   scatter plot. Here, color scales are managed using a \code{colorkey} method 
   similar to that used by the \code{\link{lattice}} function \code{\link{levelplot}}, 
   see below (arguments \code{region}, \code{colorkey}, \code{pretty}, \code{at}, 
   \code{cuts}, \code{col.regions} and \code{alpha.regions}). If \code{z} is 
   \code{NULL} or not supplied, all colors are set by \code{col} if supplied 
   or as the default lattice symbol color if both \code{z} and \code{col} 
   are not supplied.

   For \code{pchHandler}, the pch of, e.g., points on a scatter plot. 
   Here, plot symbols are managed using a reference vector  
   \code{pch.order}, but superseded by \code{pch} settings, if also 
   supplied.

   For \code{zHandler}, any vector that should to expanded by wrapping to a given 
   length, e.g. the length of the x (or y) data series to plotting.
  } 

 \item{cex, col, pch}{ For associated handlers, the parameter value(s) to be managed (i.e., 
  \code{cex} for \code{cexHandler}, etc. NOTE: In all cases if these are not 
  \code{NULL} these supersede any supplied \code{z} or 
  \code{...Handler} modification.  
  } 

 \item{cex.range}{ If supplied, the range for \code{z} to be rescaled to when using this 
  to generate a \code{cex} scale. NOTE: \code{cex.range = FALSE} disables this \code{cex} 
  scaling and uses \code{z} values directly; \code{cex.range = TRUE} applied default scaling, 
  equivalent to \code{cex.range = c(0.75, 3)}. 
  } 

 \item{region, colorkey, legend, pretty, at, cuts, col.regions, alpha.regions}{ The 
  colorscale settings to be used when generating a colorkey. The most useful of 
  these are probably \code{col.regions} which can be used to reset the color scale, 
  \code{alpha.regions} which sets the col.region alpha transparency (0 for 
  invisible to 1 for solid) and \code{colorkey} which can be a logical (forcing 
  the colorkey on or off) or a list of components that can be used to fine-tune the 
  appearance of the colorkey. 
  NOTE: The generation of colorscales is handled by \code{\link{RColorBrewer}}.
  }

  \item{pch.order}{ A vector of symbol ids (typically the numbers 1 to 24) to used 
  when plotting points if, e.g. using a scatter plot. By default, all points are 
  plotted using the first of these pch ids unless any conditioning (e.g. grouping or 
  zcase handling) is declared and linked to \code{pch}, in which symbols are assigned 
  in series from \code{pch.order}. 
  }

  \item{expand.outputs, ref}{\code{expand.outputs} is a Logical (default \code{TRUE}): 
   should outputs be expanded to the same length as \code{ref}? This can be useful if, 
   e.g., coloring points on a scatter plot that may be conditioned and therefore may 
   require \code{subscript} handling, in which case \code{ref} could be the x 
   or y data series, or any other vector of the same length. NOTE: if 
   \code{ref} is not supplied \code{expand.outputs} is ignored. 
  }

  \item{zlim}{The range over which the scale is to be applied if not \code{range(z)}.
  } 

  \item{output}{ For \code{colHandler}. The function output. Either the \code{col} vector 
   alone (\code{output='col'}) or the full list of color parameters.
  } 

  \item{...}{ Additional arguments, currently all ignored.
  } 

}


\details{
  The \code{...Handler} functions are argument handlers intended to 
   routinely handle some common activities associated with plotting 
   data. 

  \code{cexHandler} manages symbol sizes. It generates a (hopefully) 
   sensible \code{cex} scale for handling plot symbol size based on 
   a supplied input (\code{z}). 

  \code{colHandler} manages colors. It works like the colorkey in 
  \code{\link{levelplot}} in \code{\link{lattice}}, to generate a 
  colorscale based on a supplied input (\code{z}).   

  \code{zHandler} expands (by wrapping) or foreshortens vectors. 

}

\value{
  \code{cexHandler} returns a vector, which can be used as 
   the \code{cex} argument in many common plotting functions 
   (e.g. \code{\link{plot}}, \code{\link{xyplot}}).

  \code{colHandler} depending on \code{output} setting returns 
   either the \code{col} vector or a list containing elements (\code{z}, 
   \code{col}, \code{legend}, \code{at}, \code{col.regions} and 
   \code{alpha.regions}), which can be used to create a \code{col} 
   series scaled by \code{z} and an associated colorkey 
   like that generated by \code{\link{levelplot}} for other 
   \code{\link{lattice}} functions (e.g. \code{\link{xyplot}}).

  \code{pchHandler} returns a vector of \code{pch} values of an 
   appropriate length, depending on \code{expand.outputs} and \code{ref} 
   settings.
   
}

\references{
  These function makes extensive use of code developed by others.  

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

\note{
  \code{cexHandler} recently revised.

}

\seealso{
   Related functions elsewhere: \code{\link{lattice}} function \code{\link{xyplot}} 
   for the plot framework.
}

\examples{

#some trivial data
a <- 1:10
b <- a
c <- a


##example 1
##plot with cex handling

myplot1 <- function(x, y, z = NULL, cex = NULL, 
                    cex.range = NULL, ...){

    #set cex
    cex <- cexHandler(z, cex, cex.range)

    #plot
    xyplot(y~x, cex = cex,...)
}

#like plot(x, y)
myplot1(a, b)

#with z cex setting
myplot1(a, b, c)

#same as 
myplot1(a, b, c*100)

#cex rescale
myplot1(a, b, c, cex.range = c(1,5))

#cex supercedes 
myplot1(a, b, c, cex.range = c(10,50), cex = 1)



##example2
##plot function using lists/listUpdates

myplot2 <- function(x, y, z = NULL, ...){

    #my default plot
    default.args <- list(x = y~x, z = z, 
                         pch = 20, cex = 4)

    #update with whatever user supplied
    plot.args <- listUpdate(default.args, list(...))

    #col Management
    plot.args <- listUpdate(plot.args, do.call(colHandler, plot.args))
    do.call(xyplot, plot.args)
}


#with colorkey based on z/c
myplot2(a, b, c) 

#with col.regions reset
myplot2(a, b, c, col.regions = "Blues") 

#but col supersedes if supplied
myplot2(a, b, c, col = "red")


}


\keyword{ methods }

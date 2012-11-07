\name{loa-package}
\alias{loa-package}
\alias{loa}
\docType{package}
\title{
  loa 
}
\description{
  The loa package contains various plots, options and 
  add-ins for use with the \code{\link{lattice}} package.  
  
}
\details{
\tabular{ll}{
Package: \tab loa\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2011-11-12\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

\code{\link{lattice}} provides an elegant and highly powerful implementation 
of the Trellis plotting structure described by Cleveland and colleagues. 
In particular the combination of \code{panel...} functions, which can be 
layered within plots to generate novel visualisations, and simple-to-use 
conditioning make it a hugely effective tool when working with data.

The \code{loa} package contains a number of plot functions developed to make 
use of this framework. These are summarized in section 1 of this manual, and include:

1.1. \code{\link{loaPlot}} for various XYZ plots.

1.2. \code{\link{GoogleMap}} and associated geoplotting functions.

1.3. \code{\link{trianglePlot}} and associated functions.

While such 'stand alone' plot functions are of obvious value, the code framework is 
of possibly wider interest because it provides a template for the rapid third-party 
development of novel visualization functions and a highly flexible 'test bed' for 
the comparison of different data handling strategies. 

Therefore, the functions in this package have been written in a relatively dissaggregated 
fashion so code can be easily rearranged or modified by others to quickly develop 
alternative plots within the \code{\link{lattice}} framework. Firstly, plot functions 
in section 1 have where possible been supplied as main plot functions and plot component 
functions that handle data, axes, panels, etc. Secondly, the workhorse functions, those common 
functions used through-out the package to simplify many routine operations have been grouped 
together and summarized in section 2:

2.1. List handlers: \code{\link{listHandler}}, etc.

2.2. Common plot argument handlers: \code{\link{cexHandler}}, \code{\link{colHandler}}, 
\code{\link{zHandler}}, etc.

2.3. Plot lim(s) and scale(s) handlers: \code{\link{limsHandler}}, \code{\link{localScalesHandler}}, etc.

2.4. Other panel functions: \code{\link{panelPal}}, \code{\link{getArgs}}, etc.

And, finally, functions used for working with data post-plotting, are summarized in section 3: 

3.1. Interactive functions for working with plot outputs: \code{\link{getXY}}, etc.

3.2. Various functions for latitude/longitude manipulation: \code{\link{geoFrame}}, etc.

This package is very much intended to be an evolutionary exercise. I use it on a routine 
basis to develop plots for use elsewhere and compare data visualastion methods. However, 
that working pattern can generate some very 'developer-centric' code. So, I would 
be very pleased to hear from others - what they did and did not like about the 
package; what they would have liked to have been different; and, perhaps most interesting 
for me what they are using it to do. 


}
\author{
Karl Ropkins
Maintainer: Karl Ropkins <k.ropkins@its.leeds.ac.uk>
}
\references{
  Functions in \code{loa} make extensive use of code developed by others. 
  In particular, I gratefully acknowledge the huge contributions of: 

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

  Trellis Plotting:
  Becker, R. A., Cleveland, W. S., Shyu, M. J. (1996). The Visual Design and 
  Control of Trellis Display, Journal of Computational and Graphical Statistics, 
  5(2), 123-155.
  Cleveland, W.S. (1993) Visualizing Data, Hobart Press, Summit, New Jersey.
  
}

\keyword{ package }

\seealso{
  \code{\link{GoogleMap}}, \code{\link{trianglePlot}}
}

%\examples{
%############
%#to do
%}


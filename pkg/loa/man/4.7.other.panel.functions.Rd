\name{4.7.other.panel.functions}
\alias{4.7.other.panel.functions}
\alias{getArgs}
\alias{getPlotArgs}
\alias{isGood4LOA}
\alias{parHandler}

\title{
  Other panel functions argument handlers
}
\description{
  In development panel functions for use with lattice
}
\usage{

parHandler(scheme = NULL, ...)


#related

getArgs(source = TRUE, local.resets = TRUE, 
         user.resets = TRUE, is.scales.lines = FALSE, 
         elements = NULL, ..., defaults = list(), 
         defaults.only = FALSE)

getPlotArgs(defaults.as = "axis.line", source = TRUE, 
         local.resets = TRUE, user.resets = TRUE, 
         elements = NULL, ..., is.scales.lines = NULL, 
         defaults.only = TRUE)

isGood4LOA(arg)





}

\arguments{

  \item{scheme}{ The color scheme to apply. This can be a list of parameters to apply 
   or a character vector for a pre-defined scheme. Current pre-defined schemes include 
   \code{'greyscale'} (for black and white figures).
  } 

  \item{source, local.resets, user.resets}{ When recovering plot arguments 
   with \code{getArgs} or \code{getPlotArgs}, places to search for relevant 
   parameters. If supplied these would typically be vectors or lists. If vectors, 
   they are assumed to be \code{col} setting. If lists, they are assumed to be lists 
   of named parameters for inclusion. There are two cases that need to be handed 
   specially: (1) some \code{sources}, \code{local.resets} and/or \code{user.resets} 
   may contain both axis-specific and general information, e.g. For a \code{scales} list, 
   parameters to be applied just to the x axis in \code{scales$x} and parameters to be 
   applied to all scales in \code{scales}. In such cases these need to be checked in 
   order (see \code{elements} below.) (2) Some sources, e.g. axis \code{scales}, contain 
   both text and line parameters, with e.g. line settings declared as \code{col.line}, 
   etc., rather than \code{col}, etc., (which are intended for use with text.) When 
   supplied these need to be handled correctly (see \code{is.scales.lines} below). 
   \code{local.resets} and \code{user.resets} are intended as overrides for the code 
   developer and user, respectively. These can be logicals as well as vectors or lists. 
   If logicals they turn on/off the associated plot components (using \code{isGood4LOA}). 
   The check/update order is \code{source}, then \code{source$element}, then 
   \code{local.reset}, then \code{local.reset$element}, then \code{user.reset}, then 
   \code{user.reset$element}. This means that the developer always has last say regarding 
   the default appearance of a plot component and the user always has the very last say 
   from the command line if the local.reset is included as a formal argument in that plot.
   }

  \item{is.scales.lines}{ When recovering arguments with \code{getArgs} or \code{getPlotArgs}, 
   should \code{source} be treated as a lattice \code{scales} list? If so, and \code{source} 
   is checked for line parameters, line-specific terms such as \code{col.line}, etc., will be 
   recovered as \code{col}, etc., while general terms (meant for text in \code{scales} lists) 
   will be ignored. (Note: \code{getPlotArgs} guesses this based on \code{defaults.as} if 
   not supplied.)
   } 

  \item{elements}{ When recovering arguments with \code{getArgs} or \code{getPlotArgs}, this 
   identifies the elements in \code{source}, \code{local.resets} and \code{user.resets} that 
   may contain case-specific information. As with \code{lattice} handling of \code{scales} 
   axis-specific information in \code{source$element(s)} is assumed to take priority over 
   general information in \code{source}. (Note: if \code{elements} are not declared only 
   general/top level information in \code{source}, \code{local.resets} and \code{user.resets} 
   is considered at present.) 
   } 

  \item{...}{ Other arguments, often ignored.
   } 

  \item{defaults, defaults.only, defaults.as}{ When recovering arguments with \code{getArgs}, 
   \code{defaults} is an optional 'fall-back' in case nothing is recovered from \code{source}, 
   \code{local.resets} and \code{user.resets}. \code{defaults.only} is a logical: if \code{TRUE} 
   only parameters named in \code{defaults} are searched for, otherwise all parameters are 
   recovered. With \code{getPlotArgs}, \code{defaults.as} selects an appropriate default. This 
   should be a trellis parameter name, e.g. \code{'axis.line'}, \code{'axis.text'}, etc. The 
   function uses this to identify appropriate plot parameters to search for/select, e.g. 
   \code{pch}, \code{col}, \code{cex}, etc for \code{'plot.symbol'}, and to identify default 
   values for each of these (if \code{defaults.only = TRUE}).
   }

  \item{arg}{ For \code{isGood4LOA} a plot argument that can used 
   to turn a plot panel or panel component on or off. 
   }  

} 


\details{
  \code{getArgs} returns a list of parameters/values based on 
  \code{lattice}, developer and user settings. If multiple 
   elements are identified as containing case-specific information, 
   the list will contain one list of plot parameters for each named 
   element. 

   \code{getPlotArgs} is a variation of \code{getArgs} 
   intended for use with \code{panel...} and \code{l...} type 
   \code{lattice} functions. It returns a list of plot parameters 
   for different plot components, e.g. symbols, lines, or text. 

   \code{isGood4LOA} is a simple workhorse that checks if a 
   supplied \code{arg} should be used by \code{loa}. (See value and 
   note below.)

   \code{parHandler} manages the default appearance of plots.  

}

\value{
  \code{getArgs} and \code{getPlotArgs} return lists of located 
   parameters/values. For example, the call
 
  \code{getPlotArgs(default.as = "axis.line")} 

  returns a list containing the \code{lattice} defaults for an axis 
  line (\code{alpha}, \code{col}, \code{lty} and \code{lwd}) These 
  can then be used in combination with appropriate \code{x} and 
  \code{y} values in \code{llines}, or \code{panel.lines} calls. 
  The arguments \code{local.resets} and \code{user.resets} can 
  be added into the call to provide developer and user overrides. 
  (See note below.)

  \code{isGood4LOA} returns a logical (\code{TRUE} or \code{FALSE}), 
  depending on the type of a supplied argument. This returns 
  \code{FALSE} for \code{NULL}, for all \code{FALSE} logicals, and 
  any arg that has previously been tagged as 'not wanted'.

  \code{parHandler} returns a list a list suitable for use as 
  \code{par.settings} with most \code{\link{lattice}} plots. 
   
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
  \code{getPlotArgs} is intended as a 'workhorse' for plot developers, 
  to recover \code{lattice} settings, impose their own preferences on 
  these, and in turn to provide users with similar options to quickly 
  override developer settings.    

  \code{isGood4LOA} only exists because I, perhaps wrongly, 
  equate \code{arg = NULL} with \code{arg = FALSE} when that 
  argument is a component of a plot defined in the plot formals. 
  For example, in \code{\link{trianglePlot}} I want 
  \code{grids = NULL} to turn off the plot grids much like 
  \code{grids = FALSE}, but got fed up always writing the same 
  everywhere. Does not mean it is right, particularly useful or 
  even clever...

  The \code{getPlotArgs}/\code{isGood4LOA} combination is a first attempt 
  at providing plot developers with a simple tool to integrate plot argument 
  management by lattice, the plot developer and the plot user. It is intended 
  to be applied in the form shown in the Examples below. 

  Axis, tick, grid and annotation handling in \code{\link{trianglePlot}} is 
  intended to illustrate this type of application.  

}

\seealso{
   \code{\link{lattice}}, \code{\link{xyplot}},
}

\examples{

#to be confirmed

#getPlotArgs/isGood4LOA example

#in formals
#my.plot <- function(..., user.reset = TRUE, ...) 

#in main code body
#local.resets <- [what developer wants] 
#plot.arg <- getPlotArgs("[type]", source, local.reset, user.reset)

#in panel call
#(for panel off/on control)
#if(isGood4LOA(plot.arg)) panel...(..., plot.arg,...)

#in panel... function
#for panel component off/on control)
#if(isGood4LOA(plot.arg1)) panel...(..., plot.arg1,...)
#if(isGood4LOA(plot.arg2)) l...(..., plot.arg2,...)
#etc.

}


\keyword{ methods }

\name{1.1.googleMap.and.geoplotting.tools}
\alias{1.1.googleMap.and.geoplotting.tools}
\alias{googleMap}
\alias{quickMap}
\alias{makeMapArg}
\alias{getMapArg}
\alias{panel.googleMapsRaster}
\alias{panel.googleMaps}
\alias{xscale.components.googleMaps}
\alias{yscale.components.googleMaps}
\alias{axis.components.googleMaps}


\title{
  Google Maps plotting for lattice
}
\description{
  Plotting georeferenced data on maps using lattice and RgoogleMaps
}
\usage{

  googleMap(x, data = NULL, map = NULL, 
        map.panel = panel.googleMapsRaster, 
        panel = panel.xyplot, 
        recolor.map = FALSE, ...)

  quickMap(lat, lon, show.data = FALSE, ...)

  #map handlers
  makeMapArg(ylim, xlim, aspect = NULL, 
        recolor.map = FALSE, ...)
  getMapArg(object = trellis.last.object())


  #panel handlers
  panel.googleMapsRaster(map)
  panel.googleMaps(map)

  #axis handlers
  xscale.components.googleMaps(lim, ..., map = map)
  yscale.components.googleMaps(lim, ..., map = map)
  axis.components.googleMaps(map, xlim = NULL, ylim = NULL, ...)
       
}

\arguments{

  \item{x}{ (\code{googleMap} only) A formula with the general structure 
   \code{z ~ latitude * longitude | cond}. The elements \code{latitude} 
   and \code{longitude} (y and x on associated plots) are required, 
   while \code{z} and conditioning \code{cond} are optional.
   }

  \item{data}{ (\code{googleMap} only) If supplied, the assumed source 
   of the elements of formula \code{x}, typically a \code{data.frame}.
   }

  \item{map}{ (\code{googleMap} and related functions only) If supplied, 
   a modified \code{\link{RgoogleMaps}} output, to be used as the plot 
   background. If \code{NULL} (default), this is generated using 
   the \code{\link{RgoogleMaps}} function \code{\link{GetMap}}, 
   the supplied latitude, longitude ranges, and any additional 
   \code{\link{RgoogleMaps}} arguments supplied within the call. The 
   map is supplied via \code{makeMapArg} which modifies the 
   \code{\link{RgoogleMaps}} output before returning it to simplify 
   local handling and (lattice) plotting.
   }

  \item{map.panel, panel}{ (\code{googleMap} only) The panels to use 
   when generating the plot map and data layers, respectively. 
   \code{map.panel} can be the default \code{panel.googleMapsRaster} 
   or the alternative \code{panel.googleMaps}. \code{panel} 
   is by default the standard \code{lattice} scatter plot panel 
   \code{\link{panel.xyplot}}. 
   }

  \item{recolor.map}{(\code{googleMap} and \code{RgoogleMapsWrapper} 
   only). If supplied, a vector of elements that R can treat as colors, 
   used as a color scale to recolor \code{map}. This uses standard 
   \code{\link{RColorBrewer}} functions, so can handle arguments like 
   \code{recolor = c("white", "grey")} for greyscale, etc. Disabled 
   by the default \code{FALSE} or \code{NULL}.    
   }

  \item{lat,lon}{ (\code{quickMap} only) Numeric vectors of latitude and 
   longitude values.
   }

  \item{ylim, xlim, lim}{ The latitude and longitude plot ranges. \code{ylim} 
   and \code{xlim} are only required by \code{makeMapArg}, which uses these 
   to set the requested map size. For the axis handlers (\code{yscale...} and 
   \code{xscale...}) the local alternative \code{lim} is used for both 
   \code{ylim} and \code{xlim}. 
   }

  \item{aspect}{ The aspect ratio of the plot. If not supplied (recommended), this 
   is determined based on \code{map} size, but can be forced by user.
   }

  \item{show.data}{ (\code{quickMap} only) Logical, should the \code{lat}, 
   \code{lon} values supplied be plotted on the map (\code{show.data = TRUE}) 
   or just be used to define the range/size of the map being generated 
   (\code{show.data = FALSE}; default)?
   }

  \item{object}{ (\code{getMapArg} only) a \code{lattice} plot to recover an 
   \code{RgoogleMaps} map from. (If not supplied, this is assumed to last 
   lattice plot.)
   }

  \item{...}{ Additional arguments are passed on to related functions. 

   For, \code{quickMap} these are \code{makeMapArg} and the 
   \code{\link{lattice}} function \code{\link{xyplot}}. 

   For \code{googleMap} these are \code{makeMapArg}, \code{\link{cexHandler}}, 
   \code{\link{cexHandler}} and \code{\link{xyplot}}.

   \code{makeMapArg} uses the \code{\link{RgoogleMaps}} function \code{\link{GetMap}}. 
   So, most \code{GetMap} arguments can be directly accessed from either 
   \code{googleMap} or \code{quickMap} via this route, e.g. \code{maptype = "satellite"}. 
   The returned object is then modified to simplify its handling by the 
   associated panel and axis functions.
   
   By default both data point colour and size are \code{z}-scaled for \code{googleMap}. 
   If \code{z} is supplied, and \code{cex} and \code{col} are not set by the user in the 
   plot command, these plot properties are managed by \code{\link{cexHandler}} and 
   \code{\link{colHandler}}, respectively. \code{\link{cexHandler}} and 
   \code{\link{colHandler}} arguments can be passed direct as part of a \code{googleMap} 
   command to fine-tune these, e.g. \code{cex.range} to change the \code{cex} range that 
   \code{z} values are scaled to and \code{col.region} to change the color range that is 
   applied to \code{z}. See associated Help documents for further information.

   Similarly, argument passing to \code{\link{xyplot}} in both \code{googleMap} and 
   \code{quickMap} provides access to most common plot parameters, e.g. \code{col}, 
   \code{pch}, and \code{cex} for plot symbol color, type and size, respectively. 

   \code{getMapArg} recovers the \code{map} from a \code{\link{lattice}} plot object 
   generated with \code{googleMap}. Unless the plot object is supplied in the 
   \code{getMapArg} call, this is assumed to be the last lattice (trellis) output. 
   }

}

\details{
  NOTE: \code{googleMap} and related panel and axis handling functions 
   are currently in development functions and may be subject to changes.

  \code{googleMap} provides lattice-style conditioning/handling for 
   \code{\link{RgoogleMaps}} outputs. 

  \code{quickMap} is crude map plotter intended to demonstrate the use 
   of the other 'handler' functions when building dedicated mapping 
   functions.

  \code{makeMapArg} accepts latitude and longitude ranges and 
   \code{\link{RgoogleMaps}} function \code{\link{GetMap}} arguments, 
   and produces an output suitable for use with the 
   \code{panel.googleMapsRaster} and \code{panel.googleMaps} panel 
   functions or in subsequent \code{googleMap} calls if, e.g., the users 
   wishes to reuse an existing map.

  \code{panel.googleMapsRaster} and \code{panel.googleMaps} are lattice 
   panel functions that generate map layers for a lattice plot using 
   \code{makeMapArg} outputs.

  \code{yscale.components.googleMaps} and \code{xscale.components.googleMaps} 
   are y- and x-axis handlers for use with the above panels.
 
  \code{axis.components.googleMaps} is a wrapper that combines 
   \code{yscale.components.googleMaps} and \code{xscale.components.googleMaps} 
   and allows both axis to be set from the \code{lattice} function 
   argument \code{axis} rather than each individually, via 
   \code{yscale.components} and \code{xscale.components}.  

}

\value{

  \code{googleMap} and \code{quickMap} return trellis objects, 
   much like conventional \code{lattice} plot functions.

  \code{makeMapArg} returns a modified form of the 
   \code{\link{RgoogleMaps}} function \code{\link{GetMap}} 
   output suitable for use as the \code{map} argument with 
   the above functions.

  \code{getMapArg} recovers the \code{map} from an existing 
   \code{googleMap} output.

  \code{panel.googleMapsRaster} and \code{panel.googleMaps}  
   generate panel outputs suitable for use in standard \code{lattice} 
   panel functions.

  \code{yscale.components.googleMaps}, \code{xscale.components.googleMaps} 
   and \code{axis.components.googleMaps} generate suitable latitude, 
   longitude scales for use with map layers. 
   
}

\note{

  FOR THOSE NEW TO GEOREFERENCING...TO BE EXPANDED...

  Google Maps outputs are 2D projections of curve sections of 
   the Earth's surface. Therefore, the assignment of points 
   within panels and the annotation of latitudes and 
   longitudes along axis needs to be locally handled to 
   account for this.

  \code{googleMap} and \code{quickMaps} use \code{\link{RgoogleMaps}} 
   functions \code{\link{LatLon2XY}}, \code{\link{LatLon2XY.centered}} 
   and \code{\link{XY2LatLon}} to locally scale both axis and data.  

  IMPORTANT: Users wishing to add data to these plots, e.g. using 
   \code{upadate} or \code{layers} in \code{latticeExtra}, 
   should first rescale the data. See Example below. 
     
}


\references{
  This function makes extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

  RColorBrewer:
  Erich Neuwirth <erich.neuwirth@univie.ac.at> (2011). RColorBrewer:
  ColorBrewer palettes. R package version 1.0-5.
  http://CRAN.R-project.org/package=RColorBrewer

  RgoogleMaps:
  Markus Loecher and Sense Networks (2011).
  RgoogleMaps: Overlays on Google map tiles in R. R
  package version 1.1.9.6.
  http://CRAN.R-project.org/package=RgoogleMaps

}

\author{
  Karl Ropkins
}

\seealso{
   For work with (latitude, longitude) data, see \code{\link{getLatLon}} 
   regarding recovering infomation from existing plots and \code{\link{geoDistance}}, 
   etc, regarding data manipulation.    

   For information of related functions in other packages, see

   \code{\link{RgoogleMaps}}: \code{\link{GetMap}}; \code{\link{LatLon2XY}}; 
   \code{\link{LatLon2XY.centered}}; and, \code{\link{XY2LatLon}}.

   \code{\link{lattice}}: \code{\link{xyplot}}; \code{\link{panel.xyplot}}; and 
   \code{\link{panel.levelplot}}.
}

\examples{
#TO BE CONFIRMED

#quickMap code

quickMap <- function(lat, lon, show.data = FALSE, ...){

    #get map
    map <- makeMapArg(lat, lon, ...)

    #scale axis for map projection
    map.axis.comps <- axis.components.googleMaps(map)
    map.axis <- function(components, ...) 
                   axis.default(components = map.axis.comps, ...)

    #scale data for map projection
    temp <- LatLon2XY.centered(map, lat, lon)
    lat <- temp$newY
    lon <- temp$newX

    #plot data on map
    xyplot(lat~lon, 
           xlim = map$xlim, ylim =map$ylim,
           aspect = map$aspect, 
           axis = map.axis,
           panel = function(...){
               panel.googleMapsRaster(map)
               if(show.data)
                   panel.xyplot(...)
           }, ...)
}

}

\keyword{ methods }

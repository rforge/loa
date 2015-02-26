\name{1.2.GoogleMap.and.geoplotting.tools}
\alias{1.2.GoogleMap.and.geoplotting.tools}
\alias{GoogleMap}
\alias{GoogleMap.old}
\alias{googleMap}
\alias{quickMap}
\alias{makeMapArg}
\alias{getMapArg}
\alias{panel.GoogleMapsRaster}
\alias{panel.GoogleMaps}
\alias{xscale.components.GoogleMaps}
\alias{yscale.components.GoogleMaps}
\alias{axis.components.GoogleMaps}


\title{
  Google Maps plotting for lattice
}
\description{
  Plotting georeferenced data on maps using lattice and RgoogleMaps
}
\usage{

  GoogleMap(x, data = NULL, panel = panel.loaPlot,
        map = NULL, map.panel = panel.GoogleMapsRaster, 
        recolor.map = FALSE, ..., lon.lat = FALSE) 

  GoogleMap.old(x, data = NULL, map = NULL, 
        map.panel = panel.GoogleMapsRaster, 
        panel = panel.xyplot, 
        recolor.map = FALSE, ...)

  googleMap(...)

  quickMap(lat, lon, show.data = FALSE, ...)

  #map handlers
  makeMapArg(ylim, xlim, aspect = NULL, 
        recolor.map = FALSE, ...)
  getMapArg(object = trellis.last.object())

  #map panel handlers
  panel.GoogleMapsRaster(map)
  panel.GoogleMaps(map)

  #axis handlers
  xscale.components.GoogleMaps(lim, ..., map = map)
  yscale.components.GoogleMaps(lim, ..., map = map)
  axis.components.GoogleMaps(map, xlim = NULL, ylim = NULL, ...)
       
}

\arguments{

  \item{x}{ For \code{GoogleMap} and \code{GoogleMap.old} only. A 
   formula setting the plot structure, by default 
   \code{z ~ latitude * longitude | cond}.
   The axis elements \code{latitude} and \code{longitude} are required, 
   while \code{z} and conditioning \code{cond} are optional. 
   }

  \item{data}{ For \code{GoogleMap} and \code{GoogleMap.old} only. If 
   supplied, the assumed source of the elements of formula \code{x}, 
   typically a \code{data.frame}.
   }

  \item{panel, map.panel}{ For \code{GoogleMap} and \code{GoogleMap.old} 
   only. The panels to use when generating the plot data and mao layers, 
   respectively. \code{panel} is by default the standard \code{loa} 
   scatter plot panel \code{\link{panel.loaPlot}}. \code{map.panel} can 
   be the default \code{panel.GoogleMapsRaster} or the alternative 
   \code{panel.GoogleMaps}. 
   }

  \item{map}{ For \code{GoogleMap} and related functions only. If supplied, 
   a modified \code{RgoogleMaps} output, to be used as the plot 
   background. If \code{NULL} (default), this is generated using 
   the \code{RgoogleMaps} function \code{\link{GetMap}}, 
   the supplied latitude, longitude ranges, and any additional 
   \code{RgoogleMaps} arguments supplied within the call. The 
   map is supplied via \code{makeMapArg} which modifies the 
   \code{RgoogleMaps} output before returning it to simplify 
   local handling and (lattice) plotting. 
   }

  \item{recolor.map}{For \code{GoogleMap} and \code{RgoogleMapsWrapper} 
   only. If supplied, a vector of elements that R can treat as colors, 
   used as a color scale to recolor \code{map}. This uses standard 
   \code{\link{RColorBrewer}} functions, so can handle arguments like 
   \code{recolor.map = c("white", "grey")} for greyscale, etc. Disabled 
   by the default \code{FALSE} or \code{NULL}.    
   }

  \item{lon.lat}{For \code{GoogleMap} only, logical. Should the plot formula 
   be applied as \code{z ~ lon * lat | cond}? This operation is handled 
   using the \code{formula.type} argument in \code{\link{formulaHandler}}    
   }

  \item{lat,lon}{ For \code{quickMap} only. Numeric vectors of latitude and 
   longitude values.
   }

  \item{ylim, xlim, lim}{ The latitude and longitude plot ranges. \code{ylim} 
   and \code{xlim} are only required by \code{makeMapArg}, which uses these 
   to set the requested map size. For the axis handlers (\code{yscale...} and 
   \code{xscale...}) the local alternative \code{lim} is used for both 
   \code{ylim} and \code{xlim} in generic code. In \code{GoogleMap} and \code{quickMap}, if 
   supplied, \code{xlim} and \code{ylim} are passed to \code{lattice} function 
   \code{\link{xyplot}} via \code{\link{LatLon2XY.centered}} to handle local 
   scaling.
   }

  \item{aspect}{ The aspect ratio of the plot. If not supplied (recommended), this 
   is determined based on \code{map} size, but can be forced by user.
   }

  \item{show.data}{ For \code{quickMap} only, a Logical. Should the \code{lat}, 
   \code{lon} values supplied be plotted on the map (\code{show.data = TRUE}) 
   or just be used to define the range/size of the map being generated? 
   Default \code{show.data = FALSE}.
   }

  \item{object}{ For \code{getMapArg} only, a \code{lattice} plot to recover an 
   \code{RgoogleMaps} map from. (If not supplied, this is assumed to last 
   lattice plot.)
   }

  \item{...}{ Additional arguments are passed on to related functions. 

   For, \code{quickMap} these are \code{makeMapArg} and the 
   \code{\link{lattice}} function \code{\link{xyplot}}. 

   For \code{GoogleMap} these are \code{makeMapArg}, \code{\link{cexHandler}}, 
   \code{\link{cexHandler}} and \code{\link{xyplot}}.

   \code{makeMapArg} uses the \code{RgoogleMaps} function \code{\link{GetMap}}. 
   So, most \code{GetMap} arguments can be directly accessed from either 
   \code{GoogleMap} or \code{quickMap} via this route, e.g. \code{maptype = "satellite"}. 
   The returned object is then modified to simplify its handling by the 
   associated panel and axis functions.
   
   By default both data point colour and size are \code{z}-scaled for \code{GoogleMap}. 
   If \code{z} is supplied, and \code{cex} and \code{col} are not set by the user in the 
   plot command, these plot properties are managed by \code{\link{cexHandler}} and 
   \code{\link{colHandler}}, respectively. \code{\link{cexHandler}} and 
   \code{\link{colHandler}} arguments can be passed direct as part of a \code{GoogleMap} 
   command to fine-tune these, e.g. \code{cex.range} to change the \code{cex} range that 
   \code{z} values are scaled to and \code{col.region} to change the color range that is 
   applied to \code{z}. See associated Help documents for further information.

   Similarly, argument passing to \code{\link{xyplot}} in both \code{GoogleMap} and 
   \code{quickMap} provides access to most common plot parameters, e.g. \code{col}, 
   \code{pch}, and \code{cex} for plot symbol color, type and size, respectively. 

   \code{getMapArg} recovers the \code{map} from a \code{\link{lattice}} plot object 
   generated with \code{GoogleMap}. Unless the plot object is supplied in the 
   \code{getMapArg} call, this is assumed to be the last lattice (trellis) output. 
   }

}

\details{
  NOTE: \code{GoogleMap} and related panel and axis handling functions 
   are currently in development functions and may be subject to changes.

  \code{GoogleMap} provides lattice-style conditioning/handling for 
   \code{RgoogleMaps} outputs. This uses \code{\link{loaPlot}} 
   and the latest version of \code{\link{panelPal}} to manage default 
   panel and key settings.  
 

  \code{GoogleMap.old} is the previous version of the \code{GoogleMap} 
   which uses the previous version of \code{\link{panelPal}} 

  \code{googleMap} is a \code{GoogleMap} wrapper, included because this 
   alternative form of the plot name was used in earlier versions of the 
   package.

  \code{quickMap} is crude map plotter intended to demonstrate the use 
   of the other 'handler' functions when building dedicated mapping 
   functions.

  \code{makeMapArg} accepts latitude and longitude ranges and 
   \code{RgoogleMaps} function \code{\link{GetMap}} arguments, 
   and produces an output suitable for use with the 
   \code{panel.GoogleMapsRaster} and \code{panel.GoogleMaps} panel 
   functions or in subsequent \code{GoogleMap} calls if, e.g., the users 
   wishes to reuse an existing map.

  \code{panel.GoogleMapsRaster} and \code{panel.GoogleMaps} are lattice 
   panel functions that generate map layers for a lattice plot using 
   \code{makeMapArg} outputs.

  \code{yscale.components.GoogleMaps} and \code{xscale.components.GoogleMaps} 
   are y- and x-axis handlers for use with the above panels.
 
  \code{axis.components.GoogleMaps} is a wrapper that combines 
   \code{yscale.components.GoogleMaps} and \code{xscale.components.GoogleMaps} 
   and allows both axis to be set from the \code{lattice} function 
   argument \code{axis} rather than each individually, via 
   \code{yscale.components} and \code{xscale.components}.  

}

\value{

  \code{GoogleMap} and \code{quickMap} return trellis objects, 
   much like conventional \code{lattice} plot functions.

  \code{makeMapArg} returns a modified form of the 
   \code{RgoogleMaps} function \code{\link{GetMap}} 
   output suitable for use as the \code{map} argument with 
   the above functions. Note: the automatic assignment of the 
   \code{RgoogleMaps} function argument \code{size} 
   

  \code{getMapArg} recovers the \code{map} from an existing 
   \code{GoogleMap} output.

  \code{panel.GoogleMapsRaster} and \code{panel.GoogleMaps}  
   generate panel outputs suitable for use in standard \code{lattice} 
   panel functions.

  \code{yscale.components.GoogleMaps}, \code{xscale.components.GoogleMaps} 
   and \code{axis.components.GoogleMaps} generate suitable latitude, 
   longitude scales for use with map layers. 
   
}

\note{

  Google Maps outputs are 2D projections of curve sections of 
   the Earth's surface. Therefore, the assignment of points 
   within panels and the annotation of latitudes and 
   longitudes along axis needs to be locally handled to 
   account for this.

  \code{GoogleMap} and \code{quickMaps} use \code{RgoogleMaps} 
   functions \code{\link{LatLon2XY}}, \code{\link{LatLon2XY.centered}} 
   and \code{\link{XY2LatLon}} to locally scale both axis and data.  

  Important: Users wanting to add data to these plots, e.g. using 
   \code{update} or \code{layers} in \code{latticeExtra}, 
   should first rescale the data. Likewise, users wanting to add 
   maps to other plots will need to rescale plotted data to use these 
   maps. See Example 1 below.

  Important: The Google API returns a map panel larger than the 
  data (latitude, longitude) range requested. However, it does this 
  using a limited number of panel sizes. This means you may get back 
  a map that is large than necessary. As \code{xlim} and \code{ylim} 
  are passed to the API when they are called reseting these can 
  produce similar effects (so you may not get exactly the map range 
  you ask for! If you want to manually optimise the map ranges, the 
  best option is currently to start with: 

  \code{GoogleMap(..., size=c(640,640))} 

  ...and then reduce either or both of these values until you generate 
  an appropriate map size.  
       
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

   In other packages, see

   \code{RgoogleMaps}: \code{\link{GetMap}}; \code{\link{LatLon2XY}}; 
   \code{\link{LatLon2XY.centered}}; and, \code{\link{XY2LatLon}}.

   \code{\link{lattice}}: \code{\link{xyplot}}; \code{\link{panel.xyplot}}; and 
   \code{\link{panel.levelplot}}.
}

\examples{

## Example 1
## quickMap code
## as example of third-party use of functions

quickMap <- function(lat, lon, show.data = FALSE, ...){

    #get map
    map <- makeMapArg(lat, lon, ...)

    #scale axis for map projection
    map.axis.comps <- axis.components.GoogleMaps(map)
    map.axis <- function(components, ...) 
                   axis.default(components = map.axis.comps, ...)

    #scale data for map projection
    #see ?Rgooglemaps:::LatLon2XY
    temp <- LatLon2XY.centered(map, lat, lon)
    lat <- temp$newY
    lon <- temp$newX

    #plot data on map
    xyplot(lat~lon, 
           xlim = map$xlim, ylim = map$ylim,
           aspect = map$aspect, 
           axis = map.axis,
           panel = function(...){
               panel.GoogleMapsRaster(map)
               if(show.data)
                   panel.xyplot(...)
           }, ...)
}

## Example 2
## Off-line GoogleMap examples

#  Use a subsample of lat.lon.meuse
temp <- lat.lon.meuse[sample(1:155, 15),]


GoogleMap(zinc~latitude*longitude, col.regions=c("grey", "darkred"), 
          data=temp, map=roadmap.meuse)

GoogleMap(zinc~latitude*longitude, col.regions=c("grey", "darkred"),
          panel=panel.binPlot, 
          data=temp, map=roadmap.meuse)

GoogleMap(cadmium*50+copper*10+lead*2+zinc~latitude*longitude, 
          col.regions=c("grey", "darkred"), 
          key.z.main="Concentrations", panel.zcases = TRUE,
          data=temp, map=roadmap.meuse)

GoogleMap(cadmium*50+copper*10+lead*2+zinc~latitude*longitude, 
          col.regions=c("grey", "darkred"), panel=panel.zcasePiePlot,
          data=temp, map=roadmap.meuse)

#  Note 1:
#  Here, the map argument is supplied so example works off-line. 
#  If not supplied and R is on-line, GoogleMap will (try to) get map 
#  from the Google API. Repeat any of above without map argument 
#  to see. For example:
#  GoogleMap(zinc~latitude*longitude, col.regions=c("grey", "darkred"), 
#            data=lat.lon.meuse)
#  (The map will appear slightly different because non-default 
#  size and maptype settings were used to make roadmap.meuse. See 
#  ?roadmap.meuse for details.)  

#  Note 2:
#  To make a map for use with panel.GoogleMaps or panel.GoogleMapsRaster 
#  without plotting use makeMapArg(). To recover a map from a previously 
#  plotted loa GoogleMap use getMapArg().

}


\keyword{ methods }

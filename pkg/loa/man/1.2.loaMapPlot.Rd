\name{1.2.loaMapPlot.and.geoplotting.tools}
\alias{1.2.loaMapPlot.and.geoplotting.tools}
\alias{loaMapPlot}
\alias{RgoogleMapsPlot}
\alias{OpenStreetMapPlot}
\alias{GoogleMap}
\alias{googleMap}
\alias{getRGMapArg}
\alias{getOSMapArg}
\alias{panel.loaBGMapPlotRaster}
\alias{panel.loaBGMapPlot}
\alias{getMapArg}
\alias{makeMapArg}
\alias{loaMapArg}
\alias{LatLon2MercatorXY}
\alias{MercatorXY2LatLon}
\alias{axis.components.loaMap}
\alias{xscale.components.loaMap}
\alias{yscale.components.loaMap}

\title{
  Map plotting for lattice
}
\description{
  Plotting georeferenced data on maps using lattice, RgoogleMaps and OpenStreetMap
}
\usage{

  loaMapPlot(x, data = NULL, panel = panel.loaPlot, map = NULL, 
        map.panel = panel.loaBGMapPlotRaster, recolor.map = FALSE, 
        show.axes = FALSE, ..., map.source = getRGMapArg, 
        lon.lat = FALSE)

  RgoogleMapsPlot(x, data = NULL, ...)

  OpenStreetMapPlot(x, data = NULL, ...)
  
  #map handlers
  getRGMapArg(ylim, xlim, ..., lim.borders = 0.1)
  getOSMapArg(ylim, xlim, ..., lim.borders = 0.1)
  loaMapArg(object = trellis.last.object())

  #map panels
  panel.loaBGMapPlotRaster(map)
  panel.loaBGMapPlot(map)
  
  #scaling and axis handlers
  LatLon2MercatorXY(latitude, longitude, ...)
  MercatorXY2LatLon(mx, my, ...)
  axis.components.loaMap(map, xlim = NULL, ylim = NULL, ...)
  xscale.components.loaMap(lim, ..., map = map)
  yscale.components.loaMap(lim, ..., map = map)
  
  #legacy functions
  googleMap(...)
  GoogleMap(...)
  makeMapArg(...)
  getMapArg(object = trellis.last.object())
}

\arguments{

  \item{x}{ For \code{MapPlot}s only. A formula setting the plot 
   structure, by default \code{z ~ latitude * longitude | cond}.
   The axis elements \code{latitude} and \code{longitude} are required, 
   while \code{z} and conditioning \code{cond} are optional. 
   }

  \item{data}{ For \code{MapPlot}s only. If supplied, the assumed source 
   of the elements of formula \code{x}, typically a \code{data.frame}.
   }

  \item{panel, map.panel}{ For \code{MapPlots} only. The panels to use 
   when generating the plot data and map layers, respectively. \code{panel} 
   is by default the standard \code{loa} scatter plot panel 
   \code{\link{panel.loaPlot}}. \code{map.panel} can 
   be the default \code{panel.loaBGMapPlotRaster} or the alternative 
   \code{panel.loaBGMapPlot}. 
   }

  \item{map, map.source}{For \code{MapPlot}s and related functions only. 
   If supplied, \code{map} is a modified \code{RgoogleMaps} or 
   \code{OpenStreetMap} output, generated using either package and 
   \code{getRSMapArg} or \code{getOSMapArg}, respectively. If \code{NULL} 
   (default), the \code{MapPlot} using the \code{get} function assigned 
   using \code{map.source} to get the map.  
   }

  \item{recolor.map}{For \code{MapPlot}s only. If supplied, a vector of 
   elements that R can treat as colors, used as a color scale to recolor 
   \code{map}. This uses standard \code{\link{RColorBrewer}} functions, 
   so can handle arguments like \code{recolor.map} \code{= c("white", "grey")} 
   for greyscale, etc. Disabled by the default, or \code{FALSE} or \code{NULL} 
   settings.    
   }
   
  \item{show.axes}{For \code{MapPlot}s only. Logical (default \code{FALSE}) to 
   include or exclude latitude and longitude axes and associated labelling from 
   \code{MapPlot}s.    
   }

  \item{...}{ Additional arguments are passed on to related functions. 

   For \code{MapPlot}s these are \code{getRSMapArg} or \code{getOSMapArg}, 
   \code{\link{colHandler}}, \code{\link{cexHandler}} and \code{\link{xyplot}}.
   
   By default both data point colour and size are \code{z}-scaled for \code{MapPlot}s. 
   So, if \code{z} is supplied, these plot properties are managed by 
   \code{\link{cexHandler}} and \code{\link{colHandler}}, respectively, unless defined 
   in the call or \code{panel} setup. \code{\link{cexHandler}} and 
   \code{\link{colHandler}} arguments can also be passed direct as part of a 
   \code{MapPlot} command to fine-tune \code{z}-scaling, e.g. \code{cex.range} to 
   change the \code{cex} range that \code{z} values are scaled across and 
   \code{col.region} to change the color range that is applied to \code{z}. 
   See associated Help documents for further information.

   Similarly, argument passing to \code{\link{xyplot}} in both \code{MapPlot}s 
   provides access to most common plot parameters, e.g. \code{col}, 
   \code{pch}, and \code{cex} for plot symbol color, type and size, respectively. 

   \code{getMap} functions pass arguments to related \code{RgoogleMaps} or 
   \code{OpenStreetMap} functions.
   
  } 

  \item{lon.lat}{For \code{MapPlot}s only, logical. If \code{TRUE} applies 
   \code{z ~ lon * lat | cond}? This operation is handled 
   using the \code{formula.type} argument in \code{\link{formulaHandler}}    
   }

  \item{ylim, xlim, lim.borders, lim}{The latitude and longitude plot ranges. 
   \code{ylim} and \code{xlim} are only required by \code{get} functions, which 
   use these to set the requested map size. The additional argument 
   \code{lim.borders}, sets the borders to be added. The default code{0.1} adds 
   a 10\% border to both axes, while two-element vectors allow provide separate 
   control of the axes. For the axis handlers (\code{yscale...} and \code{xscale...}) 
   the local alternative \code{lim} is used for both \code{ylim} and \code{xlim} in 
   generic code.
   } 
   
  \item{object}{ For \code{loaMapArg}, the \code{MatPlot} plot to extract a 
   \code{map} from. (If not supplied, this is assumed to last \code{lattice} plot.)
   }
   
  \item{latitude, longitude}{ For \code{LatLon2MercaptorXY}, paired latitude and 
  longitude time-series to be converted to paired Mercaptor X and Y time-series.  
  }
  
  \item{mx, my}{ For \code{MercaptorXYLatLon}, paired Mercaptor X and Y time-series 
  to be converted to paired latitude and longitude time-series.  
  }
  
}

\details{

  \code{loaMapPlot} provides lattice-style conditioning/handling for 
   \code{RgoogleMaps} and \code{OpenStreetMap} outputs. This uses 
   \code{\link{loaPlot}} and \code{\link{panelPal}} to manage default 
   panel and key settings.  
 
  \code{RgoogleMapsPlot} is a shortcut for using \code{loaMapPlot} with 
   maps produced by \code{RgoogleMaps}. 
   
  \code{OpenStreetMapPlot} is a shortcut for using \code{loaMapPlot} with 
   maps produced by \code{OpenStreetMap}. 

  \code{getMap...} functions accept latitude and longitude ranges and 
   related \code{RgoogleMaps} or \code{OpenStreetMap} function arguments, 
   and produces an output suitable for use as the \code{map} argument in 
   subsequent (same area) \code{MatPlot}s or in \code{panel.loaBGMapPlotRaster} 
   or \code{panel.loaBGMapPlot} panel function calls.
   
  \code{LatLon2MercatorXY} converts supplied latitude and longitude time-series 
   to Mercator X and Y time-series. \code{MercatorXY2LatLon} converts supplied 
   Mercator X and Y time-series to latitude and longitude time-series. Both output 
   results as lists containing \code{newX} and \code{newY} elements.

  \code{panel.loaBGMapPlotRaster} and \code{panel.loaBGMapPlotMaps} are 
   \code{lattice} panel functions that generate map layers for a lattice 
   plot using \code{getMap...} function outputs.

  \code{yscale.components.loaMap} and \code{xscale.components.loaMap} 
   are y- and x-axis handlers for use with the above panels.
 
  \code{axis.components.loaMap} is a wrapper that combines 
   \code{yscale.components.loaMap} and \code{xscale.components.loaMap} 
   and allows both axis to be set from the \code{lattice} function 
   argument \code{axis} rather than each individually, via 
   \code{yscale.components} and \code{xscale.components}.

  NOTE: The \code{MapPlot}s and related panel and axis handling functions 
  are in development functions that have been subject to changes.
  
  \code{googleMap} and \code{GoogleMap} are \code{RgoogleMapsPlot} wrappers, 
  included these were names of earlier versions of this function, that have 
  been documented in previously published code.
  
  \code{getMapArg} was the earlier name for \code{loaMapArg}. 

  \code{quickMap}, a crude map plot function intended to demonstrate the use 
   of the 'handler' functions, is no longer packaged but the code is included 
   in the examples below.
}

\value{

  \code{MapPlot}s return trellis objects, 
   much like conventional \code{lattice} plot functions.

  \code{get...MapArg}s return \code{list}s for use as \code{map}s 
  in \code{...MapPlot} calls.
   

  \code{getMapArg} recovers the \code{map} from an existing 
   \code{GoogleMap} output.

  \code{panel.GoogleMapsRaster} and \code{panel.GoogleMaps}  
   generate panel outputs suitable for use in standard \code{lattice} 
   panel functions.

  \code{yscale.components.GoogleMaps}, \code{xscale.components.GoogleMaps} 
   generate suitable latitude, longitude scales for use with map layers. 
   \code{axis.components.GoogleMaps} is a wrapper for their routine use.    
}

\note{

  Google Maps outputs are 2D projections of curve sections of 
   the Earth's surface. Therefore, the assignment of points 
   within panels and the annotation of latitudes and 
   longitudes along axis needs to be locally handled to 
   account for this.

  \code{LatLon2MercatorXY} and \code{MercatorXY2LatLon} are 
   used to locally scale both axis and data in these plots. These 
   apply methods and code in the \code{"sp"} package. 

  Important: Users wanting to add data to these plots, e.g. using 
   \code{update} or \code{layers} in \code{latticeExtra}, 
   should first rescale the data. Likewise, users wanting to add 
   maps to other plots will need to rescale plotted data to use these 
   maps. See Example 1 below.

  Important: map APIs often return a map panel larger than the 
  data (latitude, longitude) range requested. This means you may get back 
  a map that is large than necessary. As \code{xlim} and \code{ylim} 
  are passed to the API when they are called reseting these can 
  produce similar effects (so you may not get exactly the map range 
  you ask for! If you want to manually optimise an \code{RgoogleMapsPlot} 
  map range, the best option is currently to start with: 

  \code{RgoogleMapsPlot(..., size=c(640,640))} 

  ...and then change either or both \code{size} values until you generate 
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
  
  OpenStreetMap:
  Ian Fellows and using the JMapViewer library by Jan Peter Stotz (2019).
  OpenStreetMap: Access to Open Street Map Raster Images. R package version 0.3.4.
  https://CRAN.R-project.org/package=OpenStreetMap
  
  sp:
  Pebesma, E.J., R.S. Bivand, 2005. 
  Classes and methods for spatial data in R. R News 5 (2). 
  https://cran.r-project.org/doc/Rnews/.

  Roger S. Bivand, Edzer Pebesma, Virgilio Gomez-Rubio, 2013. 
  Applied spatial data analysis with R, Second edition. Springer, NY. 
  http://www.asdar-book.org/
  
  Users should also always cite map sources when using outputs from 
  these \code{MapPlot}s:
  
  For GoogleMaps:
  Map layer: Map data (c) [year] Google
  
  For OpenStreetMap
  Map layer (c) OpenStreetMap contributors

}

\author{
  Karl Ropkins
}

\seealso{  

   In other packages, see

   \code{RgoogleMaps}: \code{\link{GetMap}}.
   
   \code{OpenStreetMap}: \code{\link{openmap}}.
   
   \code{sp}: \code{\link{coordinates}}; \code{\link{proj4string}}; and, 
   \code{\link{spTransform}}.
   
   \code{\link{lattice}}: \code{\link{xyplot}}; \code{\link{panel.xyplot}}; and 
   \code{\link{panel.levelplot}}.
   
}

\examples{

## Example 1
## quickMap code
## as example of third-party use of functions

quickMap <- function(lat, lon, show.data = FALSE, ...){

    #get map
    map <- getRGMapArg(lat, lon, ...)

    #scale axis for map projection
    map.axis.comps <- axis.components.loaMap(map)
    map.axis <- function(components, ...) 
                   axis.default(components = map.axis.comps, ...)

    #scale data for map projection
    temp <- LatLon2MercaptorXY(lat, lon)
    lat <- temp$newY
    lon <- temp$newX

    #plot data on map
    xyplot(lat~lon, 
           xlim = map$xlim, ylim = map$ylim,
           aspect = map$aspect, 
           axis = map.axis,
           panel = function(...){
               panel.loaGBMapPlotRaster(map)
               if(show.data)
                   panel.xyplot(...)
           }, ...)
}

## Example 2
## Off-line GoogleMap examples

#  Use a subsample of lat.lon.meuse
temp <- lat.lon.meuse[sample(1:155, 15),]

RgoogleMapsPlot(zinc~latitude*longitude, col.regions=c("grey", "darkred"), 
          data=temp, map=roadmap.meuse)

RgoogleMapsPlot(zinc~latitude*longitude, col.regions=c("grey", "darkred"),
          panel=panel.binPlot, 
          data=temp, map=roadmap.meuse)

RgoogleMapsPlot(cadmium*50+copper*10+lead*2+zinc~latitude*longitude, 
          col.regions=c("grey", "darkred"), 
          key.z.main="Concentrations", panel.zcases = TRUE,
          data=temp, map=roadmap.meuse)

RgoogleMapsPlot(cadmium*50+copper*10+lead*2+zinc~latitude*longitude, 
          col.regions=c("grey", "darkred"), panel=panel.zcasePiePlot,
          data=temp, map=roadmap.meuse)

#  Note 1:
#  Here, the map argument is supplied so example works off-line. 
#  If not supplied and R is on-line, the MapPlots will get map 
#  from either Google or OpenStreetMap API. Repeat any of above 
#  without map argument   when on-line. For example:
\dontrun{
   RgoogleMapsPlot(zinc~latitude*longitude, col.regions=c("grey", "darkred"), 
             data=lat.lon.meuse)}
#  (The map will appear slightly different because non-default 
#  size and maptype settings were used to make roadmap.meuse. See 
#  ?roadmap.meuse for details.)  

#  Note 2:
#  To make a map for use with panel.loaBGMapPlotRaster or panel.loaBGMapPlotRaster  
#  without plotting use makeRGMapArg() or makeRGMapArg(). 
#  To recover a map from a previously plotted loa MapPloy use loaMapArg().

}


\keyword{ methods }

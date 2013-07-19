\name{5.2.geo.tools}
\alias{5.2.geo.tools}
\alias{geoFrame}
\alias{geoBearing}
\alias{geoDistance}
\alias{geoElevation}
\alias{geoDestination}

\title{
  Latitude/longitude data manipulation
}
\description{
  Various functions for use with latitude, longitude values.
}
\usage{


geoFrame(lat, lon = NULL, ..., 
          pair.lat.lon = TRUE)

geoBearing(lat, lon = NULL, ...)

geoDistance(lat, lon = NULL, units = "m", 
          ...)

geoElevation(lat, lon = NULL, units = "m", 
          ...)

geoDestination(lat, lon = NULL, bearing = NULL, 
          distance = NULL, units = "m", ...)

}

\arguments{

  \item{lat,lon}{ Latitudes and longitudes to be used in geographical 
   calculations. Note: This can be supplied as two vectors or as a 
   list or data.frame containing the named vector series or components 
   \code{lat} and \code{lon}.
  } 
  \item{pair.lat.lon}{ Logical, should \code{lat} and \code{lon} series 
   be paired? If \code{TRUE} (as in default) \code{lat} and \code{lon} 
   series lengths are matched and extra values are discarded.  
  } 

  \item{units}{ The units that results should be reported in. For, 
   \code{geoDistance}, \code{geoElevation} and \code{geoDestination} 
   the current options are (the default) \code{"m"} and \code{"km"}. 
   }

  \item{bearing, distance}{ For \code{geoDestination}, the bearing 
   (relative to North) and distance to move from the start point 
   \code{lat}, \code{lon}.
   }

  \item{...}{ Additional arguments, currently ignored.

  } 

}


\details{

  \code{geoFrame} is common data handler used by other 
   \code{geo...} functions to standardize different \code{lat}, 
   \code{lon} inputs for use. 

  \code{geoBearing} calculates the bearing between different 
   latitude, longitude points, i.e., the angle clockwise from 
   North to the line formed by two consecutive points.   

  \code{geoDistance} calculates the distances between 
   different latitude, longitude points.   

  \code{geoElevation} returns the elevations of latitude, longitude 
   points, as reported by the Google Elevation api. See References 
   below.

  \code{geoDestination} returns the new latitude, longitude 
   points resulting from a move from an intial (latitude, longitude) 
   point, based on supplied distance and bearing.  
     
}

\value{
  \code{geoFrame} returns a list containing the named 
   components \code{lat} and \code{lon}. if latitude, 
   longitude pairing has been applied 
   \code{pair.lat.lon = TRUE} these components will 
   be the same length.

  \code{getBearing} returns a vector of bearings for points 
   in a supplied latitude, longitude series. Bearings (one 
   less than the number in the supplied point series) are 
   measured from prior point to latter point. 

  \code{getDistance} returns a vector of distances between 
   latitude, longitude points in a supplied series, so one 
   less the supplied number of points. By default, this 
   will be supplied in meters, but units can be modified as 
   part of the call using, e.g. \code{units = "km"} to get 
   distance(s) in kilometers.
  
  \code{getElevation} returns a vector of elevations, one for 
   each latitude, longitude point set supplied. By default, this 
   will be supplied in meters, but units can be modified as 
   part of the call using, e.g. \code{units = "km"} to get 
   elevations(s) in kilometers.

  \code{getDestination} returns a list of containing the named 
   components \code{lat} and \code{lon}. These are the destinations 
   resulting from moves from given location(s) assuming supplied 
   distance and bearing   
   
   [NOTE: this function is in development. Handle with care.]

}

\note{

  All functions currently require \code{lat} and \code{lon} inputs 
  in conventional decimal format. 

  \code{geoBearing} DETAILS NEEDED.  

  \code{geoDistance} uses the haversine formula to account to the Earth's 
   surface curavture, and uses 6371 km as the radius of earth.

  \code{geoElevation} uses the Google elevation api to get elevations, so 
   required R to into internet enabled.      

  \code{geoDestination} DETAILS NEEDED.   
}


\references{
  These functions makes extensive use of code developed by others. 

  \code{geoBearing} and \code{geoDistance} are calculated using modifications 
   of methods and JavaScript codes posted by Chris Veness. 
   http://www.movable-type.co.uk/.

  \code{geoElevation} uses the Google Elevation API. 
   http://code.google.com/apis/maps/documentation/elevation/.
 
  NEED TO COMPLETE REFS

}

\author{
  Karl Ropkins
}

\seealso{
   \code{\link{GoogleMap}} and \code{\link{getLatLon}}. 
}

\examples{

##example 1
##to be confirmed

}


\keyword{ methods }

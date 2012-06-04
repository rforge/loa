#in development code
#geographical data handlers
#[TBC - NUMBER] functions 

#geoFrame
#geoBearing
#geoDistance
#geoElevation
#geoDestination


#NOTE: 

#to do
#option for from rather than to for geoBearings
#err catcher and tidy for geoDestination


#####################
#####################
##geoFrame
#####################
#####################

#local function to manage/standarize geographic data
#[in development]

geoFrame <- function(lat, lon = NULL, ..., pair.lat.lon = TRUE){

    #lat, lon checking
    ans <- if(!is.list(lat) & !is.data.frame(lat)){
               if(is.vector(lat))
                   list(lat = lat) else
                      stop("unable to handle 'lat'", call. = FALSE)      
           } else as.list(lat)
    if(!"lon" %in% names(ans) & !is.null(lon))
        if(is.vector(lon)) 
            ans$lon <- lon else
                stop("unable to handle 'lon'", call. = FALSE)

    if(!all(c("lat", "lon") %in% names(ans)))
        stop("unable to assign 'lat' and/or 'lon', ", call. = FALSE)

    #check length?
    if(pair.lat.lon){
        temp <- min(c(length(ans$lat), length(ans$lon)))
        ans$lat <- ans$lat[1:temp]
        ans$lon <- ans$lon[1:temp]
    }
     
    #output results
    ans
    
}






#####################
#####################
##geoBearing
#####################
#####################


geoBearing <- function(lat, lon = NULL, ...){

    ans <- geoFrame(lat = lat, lon = lon, ...)

    if(length(ans$lat)<2 | length(ans$lon)<2)
        stop("need at least two lat/lon sets to calculate bearing",
             call. = FALSE)

    #setup
    deg2rad <- function(x) x * (pi/180)
    rad2deg <- function(x) x * (180/pi)

    lat2 <- ans$lat[-1]
    lat1 <- ans$lat[-length(ans$lat)]
    lon2 <- ans$lon[-1]
    lon1 <- ans$lon[-length(ans$lon)]

    #difference in longitudinal coordinates      
    dLon <- deg2rad(lon2) - deg2rad(lon1)        

    #difference in the phi of latitudinal coordinates      
    dPhi <- log(tan(deg2rad(lat2) / 2 + pi / 4) / tan(deg2rad(lat1) / 2 + pi / 4))        

    #we need to recalculate 
    #dLon if it is greater than pi      

    dLon <- ifelse(abs(dLon) > pi, 
            ifelse(dLon > 0, dLon = (2 * pi - dLon) * -1,
                             dLon = 2 * pi + dLon), 
            dLon)  



#    if(abs(dLon) > pi) {           
#        if(dLon > 0) { dLon = (2 * pi - dLon) * -1 } else 
#                     { dLon = 2 * pi + dLon } }      

    #return the angle, normalized      
    (rad2deg(atan2(dLon, dPhi)) + 360) %% 360 

}




#####################
#####################
##geoDistance
#####################
#####################


geoDistance <- function(lat, lon = NULL, units = "m", ...){

    ans <- geoFrame(lat = lat, lon = lon, ...)

    if(length(ans$lat)<2 | length(ans$lon)<2)
        stop("need at least two lat/lon sets to measure distance",
             call. = FALSE)

    #setup
    deg2rad <- function(x) x * (pi/180)
    rad2deg <- function(x) x * (180/pi)

    lat2 <- ans$lat[-1]
    lat1 <- ans$lat[-length(ans$lat)]
    lon2 <- ans$lon[-1]
    lon1 <- ans$lon[-length(ans$lon)]

    #using The haversine formula
    dLat <- deg2rad(lat2-lat1)
    dLon <- deg2rad(lon2-lon1)
    lat1 <- deg2rad(lat1)
    lat2 <- deg2rad(lat2)

    #the square of half the chord length between the points
    a <- sin(dLat/2) * sin(dLat/2) +
         sin(dLon/2) * sin(dLon/2) * cos(lat1) * cos(lat2)
 
    #the angular distance in radians
    c <- 2 * atan2(sqrt(a), sqrt(1-a)) 

    #radius of earth, km
    R <- 6371

    #scaling for output
    sc <- NULL
    if(units == "km") sc <- 1   
    if(units == "m") sc <- 1000   

#############
#handling for unrecognised units
#############   

    #output in requested scale
    R * c * sc
}



#####################
#####################
##geoElevation
#####################
#####################


geoElevation <- function(lat, lon = NULL, units = "m", ...){

    #set up frame
    ans <- geoFrame(lat = lat, lon = lon, ...)

    #make google elevation api call
    #see web guidance at
    ##http://code.google.com/apis/maps/documentation/elevation/

    temp <- paste(ans$lat, ans$lon, sep = ",", collapse = "|")
    api.call <- paste("http://maps.googleapis.com/maps/api/elevation/xml?locations=",
                      temp, "&sensor=false", sep = "")
    ans <- readLines(api.call)

    #strip out elevation information
    x <- gsub(" ", "", ans)
    x <- x[grep("elevation", x)]
    y <- gsub("</elevation>", "", x)
    y <- gsub("<elevation>", "", y)
    y<- as.numeric(y)

#may want to develop this 
#to recover other information 
#(status, lat, lon, resolution)
#and to use the interpolation 
#may also want to check out other apis
#return(ans) for list

    #units
    #scaling for output
    sc <- NULL
    if(units == "km") sc <- 0.001   
    if(units == "m") sc <- 1   

#############
#handling for unrecognised units
#############   
    
    y * sc
        
}




####################
####################
##geoDestination
####################
####################

geoDestination <- function (lat, lon = NULL, bearing = NULL, 
                      distance = NULL, units = "m", ...){

#needs tidying

    ans <- geoFrame(lat = lat, lon = lon, ...)
    deg2rad <- function(x) x * (pi/180)
    rad2deg <- function(x) x * (180/pi)

#need lots of err catchers in here
 
    lat1 <- deg2rad(ans$lat)
    lon1 <- deg2rad(ans$lon)

    bearing <- deg2rad(bearing)
 
    sc <- NULL
    if (units == "km") 
        sc <- 1
    if (units == "m") 
        sc <- 1000
    #units to km  
    distance <- distance / sc
    #Earth's radius /km
    R <- 6371 

    ang.dist <- deg2rad(distance)/deg2rad(R)

    lat2 <- asin(sin(lat1) * cos(ang.dist) + 
                 cos(lat1) * sin(ang.dist) * cos(bearing))

    lon2 <- lon1 + atan2(sin(bearing) * sin(ang.dist) * cos(lat1), 
                         cos(ang.dist) - sin(lat1) * sin(lat2))

    lat2 <- rad2deg(lat2)
    lon2 <- rad2deg(lon2)

    list(lat=lat2, lon=lon2)
}



##########################
#loaMapPlot code 
##########################

#kr 2019/09/07 started (see note) 

#note:

#replaces googleMap.R

#started because Markus changed the default RgoogleMaps output to 
#openstreetmaps so plot is not strictly GoogleMap anymore... 
#(because Google changes their sign-up conditions...)

#also because of problems using RgoogleMaps to plot on larger scales...
#(warning in his own documentation) 
#latlon2xy not a true  transform (2019/09/07)

#loaMapPlot

#kr 0.0.3 (2019/9/10)

loaMapPlot <- function(x, data = NULL, panel = panel.loaPlot, map = NULL, 
                      map.panel = panel.loaBGMapPlotRaster, 
                      recolor.map = FALSE, show.axes = FALSE, 
                      ..., map.source = getRGMapArg, lon.lat = FALSE){
  
  extra.args <- list(...)
  
  #default in panel.loaPlot is now grid=TRUE
  #this is too much clutter on maps...
  if(!"grid" %in% names(extra.args))
    extra.args$grid <- FALSE
  if(!"scheme" %in% names(extra.args))
    extra.args$scheme <- "map.data.scheme"
  #might rethink this 
  if(!show.axes){
    extra.args$scales=list(draw=FALSE)
    if(!"xlab" %in% names(extra.args))
      extra.args$xlab <- ""
    if(!"ylab" %in% names(extra.args))
      extra.args$ylab <- ""
  }

  #formular default z ~ lat * lon | cond
  if (!lon.lat) 
    if (!"formula.type" %in% names(extra.args)) 
      extra.args$formula.type = "z~y*x|cond"
    
  #make map based on data range
  temp <- do.call(formulaHandler, 
                  listUpdate(extra.args, list(x=x, data=data, 
                                              output="lattice.like")))
  myx <- if("xlim" %in% names(extra.args))
      extra.args$xlim else temp$x
  myy <- if("ylim" %in% names(extra.args))
      extra.args$ylim else temp$y
  if (is.null(map)) {
      temp <- list(xlim = myx, ylim = myy, 
                   recolor.map = recolor.map, aspect = NULL)
      temp <- listUpdate(temp, extra.args)
      map <- do.call(map.source, temp)
  }
  #recolor.map
#dedicated function
#could knock out nativeraster here? 
  if(is.logical(recolor.map) && recolor.map)
    recolor.map <- c("white", "grey")
  if(!is.null(recolor.map) & !is.logical(recolor.map)){
    ra <- dim(map$myTile)
    #if single expand to col range if possible
    if(length(recolor.map) == 1)
      recolor.map <- RColorBrewer::brewer.pal(9, recolor.map)
    #make an intensity scale
    temp <- apply(col2rgb(map$myTile), 2, prod)
    temp <- level.colors(temp, pretty(temp, 200), colorRampPalette(recolor.map)(200))
    map$myTile <- as.raster(matrix(temp, ra[1], ra[2], byrow=TRUE))
    #reset cols in frame
    #map$myTile <- level.colors(temp, pretty(temp, 200), colorRampPalette(recolor.map)(200))
    #dim(map$myTile) <- ra[1:2]
  }
  
  ##################################
  #this bit catchs maps make in RgoogleMaps 
  #rather than loa
    
  #if(any(!c("xlim", "ylim") %in% names(map))){
  #temp <- LatLon2XY.centered(map, c(map$BBOX$ll[1], map$BBOX$ur[1]), 
  #                               c(map$BBOX$ll[2], map$BBOX$ur[2]))
  #    map$xlim <- temp$newX
  #    map$ylim <- temp$newY
  #}
  #if (!"aspect" %in% names(map)) 
  #    map$aspect <- sqrt((diff(map$ylim)/diff(map$xlim))^2)
    
  ans <- do.call(loaPlot, listUpdate(list(x = x, data = data, 
                                          panel = panel), extra.args))
    
  #insert and setup map
  ans$aspect.ratio <- map$aspect
  ans$panel.args.common$xlim <- map$xlim
  ans$x.limits <- map$xlim
  ans$panel.args.common$ylim <- map$ylim
  ans$y.limits <- map$ylim
    
  #update xy elements
  #think about better handling
  #think about doing in panelPal
  #and passing coversion program so all similar
  #think about keeping lat,lon?
  if(is.null(ans$panel.args.common$x.elements))
      ans$panel.args.common$x.elements <- "x"
  if(is.null(ans$panel.args.common$y.elements))
      ans$panel.args.common$y.elements <- gsub("x", "y", ans$panel.args.common$x.elements)
  for (i in 1:length(ans$panel.args)) {
      for(j in 1:length(ans$panel.args.common$y.elements)){
        temp <- LatLon2MercatorXY(ans$panel.args[[i]][[ans$panel.args.common$y.elements[j]]], 
                                  ans$panel.args[[i]][[ans$panel.args.common$x.elements[j]]])
        ans$panel.args[[i]][[ans$panel.args.common$y.elements[j]]] <- 
          temp$newY
        ans$panel.args[[i]][[ans$panel.args.common$x.elements[j]]] <- 
          temp$newX
      }
    }
    panel <- ans$panel
    panel.with.map <- function(...) {
      map.panel(map)
      panel(...)
    }
    map.axis.comps <- axis.components.loaMap(map)
    map.axis <- function(components, ...) 
      axis.default(components = map.axis.comps, ...)
    ans <- update(ans, panel = panel.with.map, aspect = map$aspect, 
                  axis = map.axis)
    ans$panel.args.common$map <- map
    #output plot
    return(ans)
}

#other plots 

RgoogleMapsPlot <- function(x, data = NULL, ...){
  
  extra.args <- list(...)
  if(!"map.source" %in% names(extra.args))
    extra.args$map.source <- getRGMapArg
  #default to get GoogleMaps for RgoogleMaps
  #current default is http://a.tile.openstreetmap.org/
  if(!"urlBase" %in% names(extra.args))
    extra.args$urlBase <- "http://mt1.google.com/vt/lyrs=m"
  extra.args <- listUpdate(list(x=x, data=data),
                           extra.args)
  do.call(loaMapPlot, extra.args)
}

googleMap <- function(...) RgoogleMapsPlot(...)

GoogleMap <- function(...) RgoogleMapsPlot(...)

OpenStreetMapPlot <- function(x, data = NULL, ...){
  
  extra.args <- list(...)
  if(!"map.source" %in% names(extra.args))
    extra.args$map.source <- getOSMapArg
  extra.args <- listUpdate(list(x=x, data=data),
                           extra.args)
  do.call(loaMapPlot, extra.args)
}


#panels...

#panel.loaBGMapPlotRaster 

#kr v.0.0.2 (2019/09/10) (mod panel.OSMapPlotRaster)
#kr v.0.0.1 (2019/09/07) (mod panel.googleMapPlotRaster)
  
panel.loaBGMapPlotRaster <- function(map){
  grid::grid.raster(map$myTile,
              x = grid::unit(map$xlim[1], "native"), 
              y = grid::unit(map$ylim[1], "native"),
              width = grid::unit(map$xlim[2] - map$xlim[1], "native"),
              height = grid::unit(map$ylim[2] - map$ylim[1], "native"),
              just = c("left", "bottom")
  )
}

#panel.loaBGMapPlot 

#kr v.0.0.2 (2019/09/10) (mod panel.OSMapPlot + fix)
#kr v.0.0.1 (2019/09/07) (mod panel.googleMapPlot)

panel.loaBGMapPlot <- panel.OSMapPlot <- function(map){
  
  #not completely tested...
  
  #############
  #think about this
  #############
  #    if(attr(map$myTile, "type") != "local")
  #        map <- localMapManager(map)
  
  ra <- dim(map$myTile)
  map.col <- map$myTile
  xlim <- range(map$xlim, na.rm=TRUE)
  ylim <- range(map$ylim, na.rm=TRUE)
  
  map.lat  <- rep(seq(ylim[2], ylim[1],
                      length.out = ra[1]),
                  each = ra[2])
  map.lon <- rep(seq(xlim[1], xlim[2],
                     length.out = ra[2]),
                 time = ra[1])
  width <- (xlim[2] - xlim[1]) / ra[2]
  height <- (ylim[2] - ylim[1]) / ra[1]
  #could replace panel.rect with grid::grid.panel?
  #might be faster? 
  panel.rect(x = map.lon, y = map.lat,
             width = width, height = height,
             col = map.col, border = map.col)
}


#map.sources 

#getOSMapArg

# kr 0.0.2 2019/09/08 (warning supression + tidies)
# kr 0.0.1 2019/09/07 (mod makeMapArg)

getOSMapArg <- function(ylim, xlim, ..., lim.borders = 0.1){
  #more tidying to do on this
  #make a map object for OSMapPlot
  
  #create border for plot 
#border is function of arm length 
#ignores relative length of arms and aspect?
#fix in limHandler?
  temp <- limsHandler(xlim, ylim, lim.borders = lim.borders)
  xlim <- temp$xlim
  ylim <- temp$ylim
  
  #get map 
#error message from openmap??
#hiding for now; look into fixing...
  osm <- suppressWarnings(OpenStreetMap::openmap(
                                  c(ylim[2], xlim[1]), 
                                  c(ylim[1], xlim[2])))
  mytile <- matrix(osm$tiles[[1]]$colorData, 
                   ncol=osm$tiles[[1]]$yres,
                   nrow=osm$tiles[[1]]$xres,
                   byrow = TRUE)
  #make it like Markus' getMap output, so it works with existing code 
#might not need all I am tracking 
#might think about nativeraster rather than raster 
#might want to document source and supplier??? 
  mymap <- list(lat.center=mean(ylim, na.rm=TRUE), 
                lon.center=mean(xlim, na.rm=TRUE),
                zoom = MaxZoom(xlim, ylim), 
                myTile = as.raster(mytile),
                BBOX = list(ll=c(lat=ylim[1], lon=xlim[1]),
                            ur=c(lat=ylim[2], lon=xlim[2])), 
                url = "OSM",
                source = "OpenStreetMap", 
                size = c(osm$tiles[[1]]$yres, 
                         osm$tiles[[1]]$xres), 
                SCALE = 1, 
                aspect=osm$tiles[[1]]$xres/osm$tiles[[1]]$yres,
                xlim=range(c(osm$tiles[[1]]$bbox[[1]][1],
                             osm$tiles[[1]]$bbox[[2]][1])),
                ylim=range(c(osm$tiles[[1]]$bbox[[1]][2],
                             osm$tiles[[1]]$bbox[[2]][2])),
                proj = osm$tiles[[1]]$projection)
  mymap
}

#getRGMapArg

#kr v.0.0.1 (mod getOSMapArg) 

#replacing more involved earlier makeMapArg

getRGMapArg <- function(ylim, xlim, ..., lim.borders = 0.1){
  #more tidying to do on this
  #maybe rationalise with above
  #make a map object for RgoogleMapsPlot
  
  #create border for plot 
  #border is function of arm length 
  #ignores relative length of arms and aspect?
  #fix in limHandler?
  temp <- limsHandler(xlim, ylim, lim.borders = lim.borders)
  xlim <- temp$xlim
  ylim <- temp$ylim
  
  #setup
  extra.args <- list(...)
  
  #get map
  #get names of args that MapBackground can handle
  temp <- unique(c(names(formals(MapBackground)), 
                   names(formals(GetMap.bbox)),
                   names(formals(GetMap))))
  #get suitable ranges
  temp2 <- try(qbbox(lat = ylim, lon = xlim), silent = TRUE)
  if(is(temp2)[1] == "try-error")
    stop(paste("\tCould not apply supplied lat, lon combination",
               "\n\t[check call settings and data source]", sep = ""),
         call.=FALSE)
  #set size
  my.y <- diff(range(temp2$latR, na.rm=TRUE))
  my.x <- diff(range(temp2$lonR, na.rm=TRUE))
  #size was c(640, 640)
  my.size <- if(my.y > my.x)
    c(ceiling((my.x/my.y) * 640), 640) else 
      c(640, ceiling((my.y/my.x) * 640))
  #override some RgoogleMaps defaults
  map <- list(lon = temp2$lonR, lat = temp2$latR, destfile = "XtempX.png",
              maptype = "terrain", size = my.size)
  if(my.x==0 & my.y==0){
    if(is.null(map$zoom))
      map$zoom <- 15
    map$size <- c(640,640)
  }
  if(any(is.na(map$size)))
    map$size[is.na(map$size)] <- 64
  map$size[map$size < 1] <- 64
  ##update my defaults with relevant ones in call
  map <- listUpdate(map, extra.args, use.b = temp)
  #use MapBackground and list of allowed args
  map <- try(do.call(GetMap.bbox, map), silent = TRUE)
  #can't use nativeRaster at moment 
  if("nativeRaster" %in% class(map$myTile)){
    #png native output
    #must be a better way
    ra <- dim(map$myTile)
    png::writePNG(map$myTile, "XtempX.png")
    map$myTile <- png::readPNG("XtempX.png", native = FALSE)
    if(file.exists("XtempX.png")) file.remove("XtempX.png")
    attr(map$myTile, "type") <- "rgb"
    map$myTile <- rgb(map$myTile[, , 1], map$myTile[, , 2], 
                      map$myTile[, , 3], map$myTile[, , 4])
    dim(map$myTile) <- ra[1:2]
    map$myTile <- as.raster(map$myTile)
  }
  if(is(map)[1] == "try-error")
    stop(paste("\tCould not apply supplied lat, lon and RgoogleMap args",
               "\n\t[check call settings and data source]", sep = ""),
         call.=FALSE)
  temp <- LatLon2MercatorXY(
    range(c(map$BBOX[[1]][1], map$BBOX[[2]][1])),
    range(c(map$BBOX[[1]][2], map$BBOX[[2]][2]))
  )
  map$source <- "RgoogleMaps" 
  map$size <- c(dim(map$myTile)[1], dim(map$myTile)[2]) 
  map$SCALE <- 1 
  map$aspect <- dim(map$myTile)[1]/dim(map$myTile)[2]
  map$xlim <- temp$newX
  map$ylim <- temp$newY
  #            ylim=range(c(map$BBOX[[1]][2],
  #                         map$BBOX[[2]][2]))
  #assumng for now
  map$proj = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
  
  #stopping here for now...
  return(map)
}

#old name 
makeMapArg <- function(...) getRGMapArg(...)

#local gets

loaMapArg <- function(object = trellis.last.object()){
  #recovers map from previous loa Map plot
  object$panel.args.common$map
}

#old name 
getMapArg <- function(object = trellis.last.object()){
  #recovers map from previous loa Map plot
  object$panel.args.common$map
}

#LatLon2MercatorXY

# kr 0.0.1 2019/09/07
# kr 0.0.2 2019/09/09 (NA handling)

LatLon2MercatorXY <- function(latitude, longitude, ...){
  #should be able to simplify at some stage
  #but projectMercator cannot handle NAs
  #so need to track and handle
  if(length(latitude)>length(longitude))
    longitude <- c(longitude, 
                   rep(NA, length(latitude)))[1:length(latitude)]
  if(length(latitude)<length(longitude))
    latitude <- c(latitude, 
                  rep(NA, length(longitude)))[1:length(longitude)]
  df <- data.frame(ref=1:length(latitude),
                  latitude=latitude, longitude=longitude)
  df <- na.omit(df)
#might need a catch here if no valid cases in d...
  #############################
  #from projectMercator
  sp::coordinates(df) <- ~longitude + latitude
  sp::proj4string(df) <- sp::CRS("+proj=longlat +datum=WGS84")
  df1 <- sp::spTransform(df, osm())
  loc <- sp::coordinates(df1)
  ##############################
  newX <- rep(NA, length(latitude))
  newX[df$ref] <- loc[,1]
  newY <- rep(NA, length(latitude))
  newY[df$ref] <- loc[,2]
  list(newX = newX, newY=newY)
}

#might need 
# LatLon2MercatorXY.centered (not sure what this does)

# MercatorXY2LatLon

# kr v.0.0.1 2019/09/10 (mod of LatLon2MercatorXY)

MercatorXY2LatLon <- function(mx, my, ...){
  #should be able to simplify at some stage
  #but projectMercator cannot handle NAs
  #so need to track and handle
  if(length(my)>length(mx))
    mx <- c(mx, rep(NA, length(my)))[1:length(my)]
  if(length(my)<length(mx))
    my <- c(my, rep(NA, length(mx)))[1:length(mx)]
  df <- data.frame(ref=1:length(my),
                   my=my, mx=mx)
  df <- na.omit(df)
  #might need a catch here if no valid cases in d...
  #############################
  #MOD from projectMercator
  sp::coordinates(df) <- ~mx + my
  sp::proj4string(df) <- 
    sp::CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
  df1 <- sp::spTransform(df, sp::CRS("+proj=longlat +datum=WGS84"))
  loc <- sp::coordinates(df1)
  ##############################
  newX <- rep(NA, length(my))
  newX[df$ref] <- loc[,1]
  newY <- rep(NA, length(my))
  newY[df$ref] <- loc[,2]
  list(newX = newX, newY=newY)
}


#axes

#kr 0.0.1 2019/09/09 (MOD axis.components.OSMap, etc)

axis.components.loaMap <- function(map, xlim = NULL, ylim = NULL, ...){
  #the dots currently go no further!
  if(is.null(xlim))
    xlim <- map$xlim
  if(is.null(ylim))
    ylim <- map$ylim   
  #get and combine
  ans <- xscale.components.loaMap(xlim, map = map)
  ans <- listUpdate(ans, yscale.components.loaMap(ylim, map = map)) 
  ans
}

#xscale

xscale.components.loaMap <- function(lim, ..., map = map){
  ans <- xscale.components.default(c(map$BBOX$ll[2], map$BBOX$ur[2]), ...)
  ans$num.limit <- map$xlim
  #could fix this in LatLon2MercatorXY
  len <- length(ans$bottom$ticks$at)
  temp <- LatLon2MercatorXY(rep(map$BBOX$ll[1], len), 
                            ans$bottom$ticks$at)
  temp.2 <- LatLon2MercatorXY(rep(map$BBOX$ur[1], len), 
                              ans$bottom$ticks$at)
  ans$bottom$ticks$at <- temp$newX
  ans$bottom$labels$at <- temp$newX
  ans$top <- ans$bottom
  ans$top$ticks$at <- temp.2$newX
  ans$top$labels$at <- temp.2$newX
  ans
}

#yscale

yscale.components.loaMap <- function(lim, ..., map = map){
  ans <- yscale.components.default(c(map$BBOX$ll[1], map$BBOX$ur[1]), ...)
  ans$num.limit <- map$ylim
  #could fix this in LatLon2MercatorXY
  len <- length(ans$left$ticks$at)
  temp <- LatLon2MercatorXY(ans$left$ticks$at, 
                            rep(map$BBOX$ll[2], len))
  temp.2 <- LatLon2MercatorXY(ans$left$ticks$at, 
                              rep(map$BBOX$ur[2], len))
  ans$left$ticks$at <- temp$newY
  ans$left$labels$at <- temp$newY
  ans$right <- ans$left
  ans$right$ticks$at <- temp.2$newY
  ans$right$labels$at <- temp.2$newY
  ans
}


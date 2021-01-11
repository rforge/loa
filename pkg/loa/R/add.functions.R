#indevelopment

###################
#add functions
###################

######################
#function (exported)
######################

# add.loaGhosts (exported)
# add.Y2Axis (exported)
# add.XYPolygon (exported)
# add.lonLatMap (in development)

# add_loaPanel (unexported)

#########################
#to do
#########################

#lot of stuff



########################
#functions
########################

########################
#add.loaGhosts
########################


#add ghost points in multipanel loaPlots

add.XYZGhosts <- function (object = trellis.last.object(), 
                           ..., unit = "native", 
                           ghost.panel = panel.loaPlot){ 

##############################
#to look at 
##############################
#like to move this ghost.panel to panel.loa 
#  but that generates warning missing points
#  also type handling like panel.loaPlot... 

  
  #add ghost points to an existing plot
  extra.args <- list(...)
  temp <- object$panel.args
  if(length(temp)==1) warning("ghosts might not help...")
  temp2 <- lapply(c("x", "y", "z"), function(i){
    temp <- lapply(temp, function(x) x[[i]])
    do.call(c, temp)
  })
  names(temp2) <- c("x", "y", "z")
  if("type" %in% names(object$panel.args.common)){
    temp2[["type"]] <- object$panel.args.common$type
  }
  temp2[["col"]] <- "grey"
  temp2[["alpha"]] <- 0.25
  panel <- object$panel
  object$panel <- function(...){
    do.call(ghost.panel, listUpdate(temp2,
                                    extra.args))
    panel(...)
  }
  object
}




########################
#add.Y2Axis
########################

#add a Y2 scale to an existing plot

add.Y2Axis <- function (object = trellis.last.object(), 
                        ..., unit = "native", 
                           rescale = NULL){ 
  #setup
  x.args <- list(...)
  if(is.null(rescale)){
    rescale <- 1
  }

  #plot reset
  object$y.scales$alternating <- 3
  ref <- object$yscale.components
  object$yscale.components <- function(lim, ...){
    ans <- ref(lim = lim, ...)
    ans$right <- ans$left
    ans$right$labels$labels <- as.character(ans$right$labels$at * rescale)
    ans
  }
  
  #right ylab and col
  if("ylab" %in% names(x.args)){
    temp <- if("col" %in% names(x.args))
      list(x.args$ylab, col=x.args$col) else 
        x.args$ylab
    object$ylab.right <- temp
  }
  if("col" %in% names(x.args)){
    object$axis <- function(side, text.col=NULL, line.col=NULL,
                            ...) {
        if(is.null(line.col)) line.col <- "black"
        if(is.null(text.col)) text.col <- "black"
        # colour right separately
        if(side %in% c("left","bottom", "top")) {
          axis.default(side = side, text.col=text.col, 
                       line.col=line.col, ...)
        } else {
          line.col <- x.args$col
          text.col <- x.args$col
          axis.default(side = side, text.col=text.col, 
                       line.col=line.col, ...)
        }
      
    }
  }
  #hold rescale
  object$panel.args.common$loa.y2.rescale=rescale
  #output
  object
    
}



########################
#add.XYPolygon
########################

#add a Y2 scale to an existing plot

add.XYPolygon <- function (object = trellis.last.object(),
                           x = NULL, y = NULL, data = NULL,
                           ..., unit = "native",
                           y2.scale=FALSE, first=FALSE){ 
  
  #setup 
  x.args <- listUpdate(list(col="grey", border=NA), 
                       list(...))
  #########################
  #testing
  argnames <- names(as.list(match.call(expand.dots = TRUE)[-1]))
  arguments <- as.list(match.call()[-1])
  if(!is.null(data)) data <- as.data.frame(data)
  env <- parent.frame()
  x.name <- as.character(arguments)[argnames == "x"]
  y.name <- as.character(arguments)[argnames == "y"]
  #groups.name <- as.character(arguments)[argnames == "groups"]
  #cond.name <- as.character(arguments)[argnames == "cond"]
  df <- list(x = eval(substitute(x), data, env), 
             y = eval(substitute(y), data, env))#, 
             #groups = eval(substitute(groups), data, env), 
             #cond = eval(substitute(cond), data, env))
  ###############################
  df <- listUpdate(df, x.args)
  
  if(y2.scale){
    if("loa.y2.rescale" %in% names(object$panel.args.common)){
      df$y <- df$y * 1/object$panel.args.common$loa.y2.rescale
    } else {
      #warning needs tidying
      warning("no second y2")
    }
  }

  #add polygon panel
  panel <- object$panel
  if(first){
    object$panel <- function(...){
      do.call(panel.polygon, df)
      panel(...)
    }
  } else {
    object$panel <- function(...){
      panel(...)
      do.call(panel.polygon, df)
    }
  }
  object

}


########################
#add.lonLatMap
########################

#add a map layer to a map

#notes
############################
#adds at back 
#but rerunning put new map behind old
#might want to think about keeping pre-map panel
#also transforms data


add.LonLatMap <- function (object = trellis.last.object(),
                        ..., map = NULL, recolor.map=FALSE,
                        show.axes = FALSE, unit = "native",
                        first = TRUE){ 
  #draw and add map layer

  #test this is not multiscale
  if(!is.numeric(object$x.limits) & 
     !is.numeric(object$y.limits)){
    #think this is free scale 
    stop("looks multiscale; not adding map...")
  }  
  
  if(is.null(object$loa)){
    object$loa <- list(transformed="not")
  }
  if(is.null(object$loa$xlim)){
    object$loa$xlim <- object$x.limits
  }
  if(is.null(object$loa$ylim)){
    object$loa$ylim <- object$y.limits
  }
  xlim <- object$loa$xlim
  ylim <- object$loa$ylim
  
  #setup 
  x.args <- listUpdate(list(xlim = xlim, ylim = ylim,
                            map.source = getOSMapArg,
                            map.panel = panel.loaBGMapPlotRaster), 
                       list(...))
  if(is.null(map)){
    map <- do.call(x.args$map.source, x.args)
  }
  #recolor map
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
  
  #rescale plot for map + 
  
  #insert and setup map
  object$aspect.ratio <- map$aspect
  object$panel.args.common$xlim <- map$xlim
  object$x.limits <- map$xlim
  object$panel.args.common$ylim <- map$ylim
  object$y.limits <- map$ylim
  
  #reset x and y as 
  #need next bit in x0 and x1 or x.high and x.low, etc in there
  if(is.null(object$panel.args.common$x.elements))
    object$panel.args.common$x.elements <- "x"
  if(is.null(object$panel.args.common$y.elements))
    object$panel.args.common$y.elements <- gsub("x", "y", 
                          object$panel.args.common$x.elements)
  #now transform all x and y if not transformed
  if(object$loa$transformed=="not"){
    for (i in 1:length(object$panel.args)) {
      for(j in 1:length(object$panel.args.common$y.elements)){
        temp <- LatLon2MercatorXY(object$panel.args[[i]][[object$panel.args.common$y.elements[j]]], 
                                  object$panel.args[[i]][[object$panel.args.common$x.elements[j]]])
        object$panel.args[[i]][[object$panel.args.common$y.elements[j]]] <- 
          temp$newY
        object$panel.args[[i]][[object$panel.args.common$x.elements[j]]] <- 
          temp$newX
      }
    }
    object$loa$transformed <- "Mercator"
  }
  .panel <- object$panel
  map.panel <- x.args$map.panel
  if(is.character(.panel)) {
    .panel <- get(.panel)
  }
  
  panel.with.map <- if(first){
    function(...) {
      map.panel(map)
      .panel(...)
    }
  } else {
    function(...) {
      .panel(...)
      map.panel(map)
    }
  }

  map.axis.comps <- axis.components.loaMap(map)
  map.axis <- function(components, ...) 
    axis.default(components = map.axis.comps, ...)
  object <- update(object,  panel=panel.with.map,
                aspect = map$aspect, 
                axis = map.axis)
  object$panel.args.common$map <- map
  
  if(!show.axes){
    object <- update(object, xlab="", ylab="",
                     scales=list(draw=FALSE))
  }

  return(object)
  
}






########################
#add_loaPanel
########################

add_lonLatPanel <- function(lattice.plot=trellis.last.object(),
                         preprocess = NULL,
                         panel =NULL, postprocess = NULL,
                      ...){
  x.args <- list(...)
  if(!is.null(preprocess)) 
    lattice.plot <- do.call(preprocess, listUpdate(x.args,
                                 list(lattice.plot=lattice.plot)))
  x.args <- listUpdate(x.args, list(type="n", grid=FALSE))
  if(!is.null(panel)){
    pre.panel <- lattice.plot$panel
    lattice.plot$panel <- function(...){
      pre.panel(...)
      #pass args in add... function to this panel?
      do.call(panel, listUpdate(list(...), x.args))
    }
  }
  if(!is.null(postprocess)) 
    lattice.plot <- do.call(postprocess, listUpdate(x.args,
                                 list(lattice.plot=lattice.plot)))
  lattice.plot  
}





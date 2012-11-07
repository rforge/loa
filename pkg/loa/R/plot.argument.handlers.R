#in development code
#[TBC - NUMBER] functions 


#cexHandler
#colHandler
#zHandler

#[in-development]
#parHandler

#to think about



#TO DO urgent
###############
#in getPlotArgs
#URGENT quickfix in place 
#needs a rewrite
#also think about order of formals



#to do
###############
#think about hardcoded defaults
#on col and cex


#to think about
###############
#might be better replacing 
#zhandler with listExpand
#


#NOTE: much borrowed from lattice 

#####################
#####################
##cexHandler
#####################
#####################

#local function to handler cex
#when supplying a z argument to xyplot
#[in development]



cexHandler <- function(z = NULL, cex = NULL, 
         cex.range = NULL, expand.outputs = TRUE, 
         ref = NULL, ..., zlim = NULL){

    #if cex there use it
    if(!is.null(cex))  
        return(zHandler(cex, expand.outputs, ref))

    if(is.null(cex.range))
        cex.range <- TRUE
    
    cex <- z
        
    #cex.range setup
    if(is.logical(cex.range) && cex.range)
        cex.range <- c(2, 10)
    if(is.numeric(cex.range)){
        temp <- range(cex.range, na.rm = TRUE, finite = TRUE)
        cex.range <- if(length(na.omit(temp)) < 2)
                         FALSE else temp
    }

    #cex default 
    cex.ref <- min(cex.range, na.rm=TRUE) + (0.2 * (max(cex.range, na.rm=TRUE)- min(cex.range, na.rm=TRUE)))



    if(is.null(cex) || length(na.omit(cex))<1){
        cex <- if(is.numeric(cex.range)) cex.ref else 1
    } else {
        cex <- as.numeric(cex)

        if(!is.null(zlim)){
              temp <- range(as.numeric(zlim), na.rm = TRUE, finite = TRUE)
              cex[cex < min(zlim, na.rm=TRUE) | cex > max(zlim, na.rm=TRUE)] <- NA
        } else {
              temp <- range(cex, na.rm = TRUE, finite = TRUE)
        }

        #temp <- range(cex, na.rm = TRUE, finite = TRUE)


        my.range <- if(length(na.omit(temp)) < 2)
                        FALSE else temp
        if(is.numeric(cex.range)){
            if(my.range[1] == my.range[2]){
                cex[cex == my.range[1]] <- cex.ref
            } else {
                temp <- (cex.range[2]-cex.range[1]) / (my.range[2]-my.range[1])
                cex <- cex.range[1] + (cex - my.range[1]) * temp
            }
        }
    }
 
    #return cex 
    #scaled by ref if requested
    return(zHandler(cex, expand.outputs, ref))

}



#####################
#####################
##colHandler
#####################
#####################

#local function to handler col and colorkey
#when supplying a z argument to xyplot
#[in development]


colHandler <- function(z = NULL, col = NULL, 
         region = NULL, colorkey = FALSE, legend = NULL,
         pretty = FALSE, at = NULL, cuts = 20,
         col.regions = NULL, alpha.regions = NULL,
         expand.outputs = TRUE, ref = NULL, 
         ..., zlim = NULL, output="col"){


    #check par.settings
    if(is.null(col.regions) & !is.null(list(...)$par.settings))
        col.regions <- list(...)$par.settings$regions$col

    #if col.regions supplied
    if(!is.null(col.regions) && length(col.regions) < 100){
        if(length(col.regions)==1)
            col.regions <- brewer.pal(9, col.regions)
         col.regions <- colorRampPalette(col.regions)(100)
   }

   #if both z and col present
   #ignore z when coloring data
   if(!is.null(z) & !is.null(col)){

#silence this warning
#properly
#       warning("z and col both given, z ignored when coloring data.", call. = FALSE)
   }

   if(length(z)<1) z <- NULL

   #if neither z nor col available
   #create defaults
   if(is.null(z) & is.null(col)){
        if(is.null(col.regions))
            regions <- FALSE
        if(is.null(col.regions))
            regions <- FALSE
        col <- trellis.par.get("dot.symbol")$col
   } 

   #z handle if not col
   #else col handle
   if(is.null(col)){
        #just z given
        if(!is.numeric(z)){
            z <- as.factor(z)
            zlim <- z
        } else {
            if(!is.null(zlim)){
                z[z < min(zlim, na.rm=TRUE) | z > max(zlim, na.rm=TRUE)] <- NA
            } else {
                zlim <- range(z, finite=TRUE)
            }
        }
        zrng <- lattice:::extend.limits(range(as.numeric(zlim), finite = TRUE))
        if(is.null(at)) 
            at <- if(pretty) 
                      pretty(zrng, cuts) else 
                          seq(zrng[1], zrng[2], length.out = cuts + 2)
        if(is.null(col.regions)){
            col <- level.colors(z, at)
            col.regions <- colorRampPalette(level.colors(at, at))(100)
        } else col <- level.colors(z, at, col.regions)
    } else {
        #all other cases use
        #using col
        z <- if(is.numeric(col)) col else as.factor(col)
        zrng <- lattice:::extend.limits(range(as.numeric(z), finite = TRUE))
        if(is.null(at)) 
            at <- if(pretty) 
                     pretty(zrng, cuts) else 
                          seq(zrng[1], zrng[2], length.out = cuts + 2)
   }

   #creat col.regions if still missing
   if(is.null(col.regions)){
       temp <- if(is.numeric(col)) sort(col) else col
       temp <- unique(temp)
       temp <- if(length(temp) == 1) c(temp, temp) else temp 
       col.regions <- colorRampPalette(level.colors(at, at, 
                          colorRampPalette(temp)(100)))(100)
   }                       

   #handle alpha
   if(is.null(alpha.regions))
        alpha.regions <- 1

   if(is.logical(alpha.regions) && alpha.regions)
        alpha.regions <- 0.5
   
   if(is.numeric(alpha.regions)){
       if(any(alpha.regions < 0) | any(alpha.regions > 1)){
           warning(paste("could sensibly apply requested 'alpha.regions'",
                         "\n\t[Suggest numeric in range 0 to 1]", 
                         "\n\t[resetting out-of-range value(s) to 0 or 1]",
                    sep=""), call.=FALSE)
            alpha.regions[alpha.regions > 1] <- 1
            alpha.regions[alpha.regions < 0] <- 0
        }
        col <- col2rgb(col)
        col <- rgb(col[1,], col[2,], col[3,], 
                      alpha = alpha.regions * 255, max = 255)
    }    

    #show regions,colorkey if
    #if a col range to work with and either is NULL 
    if(length(unique(col))>1){
        if(is.null(region))
            region <- TRUE
        if(is.null(colorkey))
            colorkey <- TRUE 
    }

    #set up colorkey

#could move defaults out into call?

    if (is.logical(colorkey)) {
        if (colorkey) {
            colorkey <- list(at = at, space = "right")
            if (!is.null(col.regions)) 
                colorkey$col <- col.regions
            if (!is.null(alpha.regions)) 
                colorkey$alpha <- alpha.regions
        }
        else colorkey <- NULL
    } else if (is.list(colorkey)) {
            tmp <- list(space = if (any(c("x", "y", "corner") %in% 
                names(colorkey))) "inside" else "right", at = at)
            if (!is.null(col.regions)) 
                tmp$col <- col.regions
            if (!is.null(alpha.regions)) 
                tmp$alpha <- alpha.regions
            colorkey <- listUpdate(tmp, colorkey)
    } else {
         if(!is.null(colorkey)){
             warning("unexpected colorkey ignored.", call. = FALSE)
             colorkey <- NULL
         }
    }

    #structure like legend
    if(!is.null(colorkey)){
        colorkey <- list(right = list(fun = draw.colorkey,
                         args = list(key = colorkey),
                         draw =FALSE))
       if("space" %in% names(colorkey$right$args$key))    
           names(colorkey)[[1]] <- colorkey$right$args$key$space
    }

    #legend update if not preset
    if(is.null(legend))
          legend <- colorkey

    #expand z,col if requests and 
    #ref to scale by...
    col <- zHandler(col, expand.outputs, ref)
    z <- zHandler(z, expand.outputs, ref)

#print(col)

    #return relevant settings
    if(output=="col") return(col)
    list(z = z, col = col, 
         legend = legend, at = at, 
         col.regions = col.regions, 
         alpha.regions = alpha.regions)

}



#####################
#####################
##zHandler
#####################
#####################

#local function to handler simple cases
#expand to ref length
#[in development]

zHandler <- function(z = NULL, expand.outputs = TRUE, 
          ref = NULL, ...){
                
    if(is.null(z)) return(z)
                      
    if(is.null(ref) | !expand.outputs) return(z)
    if(length(z) < length(ref))
        rep(z, ceiling(length(ref)/length(z)))[1:length(ref)] else 
            z[1:length(ref)]
}




########################
########################
##parHandler
########################
########################

parHandler <- function (scheme = NULL, ...) {


#######################
#urgent
#######################
#rethink the inputs
#

#think about order
#theme first means unnamed get assigned as this

#could hide this in ...
#then
#if theme named
#if output set par.settings
#

#could use theme and set.theme
#then if garbage sent it will ignore

    extra.args <- list(...)

    ###################
    #pre-defined themes
    ###################

    myschm <- NULL

    if(is.character(scheme)){

        if (scheme == "greyscale") {
            symbol <- gray(1:8/8)
            fill <- "grey"
            region <- gray(11:1/11)
            reference <- "black"
            bg <- "transparent"
            fg <- "black"
            myschm <- list(plot.polygon = list(col = fill[1], border = fg[1]), 
                         box.rectangle = list(col = symbol[1]), box.umbrella = list(col = symbol[1]), 
                         dot.line = list(col = reference), dot.symbol = list(col = symbol[1]), 
                         plot.line = list(col = symbol[1]), plot.symbol = list(col = symbol[1]), 
                         regions = list(col = colorRampPalette(region)(100)), 
                         reference.line = list(col = reference), superpose.line = list(col = symbol), 
                         superpose.symbol = list(col = symbol), superpose.polygon = list(col = fill, 
                         border = fg), background = list(col = bg), add.line = list(col = fg), 
                         add.text = list(col = fg), box.dot = list(col = fg), 
                         axis.line = list(col = fg), axis.text = list(col = fg), 
                         strip.border = list(col = fg), strip.background = list(col = "white"), 
                         strip.shingle = list(col = "grey"), box.3d = list(col = fg), 
                         par.xlab.text = list(col = fg), par.ylab.text = list(col = fg), 
                         par.zlab.text = list(col = fg), par.main.text = list(col = fg), 
                         par.sub.text = list(col = fg))
        }

        if (scheme == "kr.web") {
            symbol <- colHandler(1:8, col.regions="Blues")$col
            fill <- "white"
            region <- colHandler(1:11, col.regions="Blues")$col
            reference <- "white"
            bg <- "black"
            fg <- "white"
            myschm <- list(plot.polygon = list(col = fill[1], border = fg[1]), 
                         box.rectangle = list(col = symbol[1]), box.umbrella = list(col = symbol[1]), 
                         dot.line = list(col = reference), dot.symbol = list(col = symbol[1]), 
                         plot.line = list(col = symbol[1]), plot.symbol = list(col = symbol[1]), 
                         regions = list(col = colorRampPalette(region)(100)), 
                         reference.line = list(col = reference), superpose.line = list(col = symbol), 
                         superpose.symbol = list(col = symbol), superpose.polygon = list(col = fill, 
                         border = fg), background = list(col = bg), add.line = list(col = fg), 
                         add.text = list(col = fg), box.dot = list(col = fg), 
                         axis.line = list(col = fg), axis.text = list(col = fg), 
                         strip.border = list(col = fg), strip.background = list(col = "black"), 
                         strip.shingle = list(col = "lightgrey"), box.3d = list(col = fg), 
                         par.xlab.text = list(col = fg), par.ylab.text = list(col = fg), 
                         par.zlab.text = list(col = fg), par.main.text = list(col = fg), 
                         par.sub.text = list(col = fg))
        }

    }

    if(is.list(scheme))
        myschm <- scheme
    
    if(is.list(myschm)){
        if (is.null(extra.args$par.settings)) 
            extra.args$par.settings <- myschm
        else extra.args$par.settings[!names(myschm) %in% names(extra.args$par.settings)] <- myschm[!names(myschm) %in% 
            names(extra.args$par.settings)]
    }

#warning if myschm not list?

#return par.setting or extra.args?

    extra.args$par.settings
}






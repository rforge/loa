#in development code
#[TBC - NUMBER] functions 

#trianglePlot 
#panel.triangleAxes
#panel.triangleFrame
#panel.triangleGrids
#triABC2XY
#triXY2ABC







##################################
#################################
#################################


#args orders messed up in doc

#need to add in getPlotArgs

#################################
##################################
##
#############################


#to do
####################
#tweak label positioning
#triABC2Xy tidy 
#triABCLims dedicated function
#triXY2ABC tidy
#getTriABC function for interactives
#arrow in panel.scaleHandler
#

#like to do better
####################
#axis annotation/lab size handling
#

#to think about
##################
#move the label handling 
#into panelscale
#add a scaling factor
#based on number of panels?
####################
#allowing tck, at, etc 
#access via getPlotArgs
#so they can do everything???
#
####################
#migth want to remove the following 
#by default but allow if supplied:
#xlab, ylab,
#(Currently can't override)
#####################
#maybe needed plot prop need 
#more rigorous setting?
#                 col = add.line$col, fill = col, alpha = add.line$alpha,
#                 lty = add.line$lty, lwd = add.line$lwd,
#                 col.text = add.text$col, alpha.text = add.text$alpha
#####################

#CITE: much borrowed from lattice 
#CITE: triangle plot methods

#get from help


##############################
##############################
##trianglePlot
##############################
##############################

#notes 
##########################
#localScalesHandler and panel.localScale 
#to generate triangle axis
###


trianglePlot <- function(x, data = NULL, panel = panel.xyplot, ref.grids = TRUE, 
                         ..., axes = TRUE, ticks = TRUE, grids = TRUE, annotation = TRUE
                         ){

    ########
    #setup
    ########

    #extra.args
    extra.args <- list(...)

    #kill x/ylabs
    #user can use these
    extra.args$xlab <- ""
    extra.args$ylab <- ""
    
    ##################
    #see make plot later 
    #for plot defaults
    ##################
    #aspect, etc.

    #update scales
    temp <- listUpdate(extra.args, list(allowed.scales = c("a", "b", "c"), 
                                        disallowed.scales = c("x", "y"), 
                                        remove.box = TRUE))
    #this might need rethinking
    extra.args <- listUpdate(extra.args, do.call(localScalesHandler, temp), 
                             ignore.a = "scales")

    #################
    #ref.grids, grids, etc.
    #################

    #ref.grids handling
    if(is.null(ref.grids)) 
        ref.grids <- FALSE

    if(is.logical(ref.grids))
        ref.grids <- if(all(ref.grids))
                        c("darkgreen", "darkred", "darkblue") else "black" 
    ref.grids <- zHandler(ref.grids, TRUE, 1:3)

    #for grid, annotation slightly different 

    local.axis <- list(lty = 1, alpha = 0.5, col = "black") 

    local.ticks <- list(lty = 1, 
                        a = list(col = ref.grids[1]),
                        b = list(col = ref.grids[2]),
                        c = list(col = ref.grids[3]))

    local.grids <- list(lty = 2, alpha = 0.5, 
                        a = list(col = ref.grids[1]),
                        b = list(col = ref.grids[2]),
                        c = list(col = ref.grids[3]))

    local.ann <- list(cex = 0.8, 
                      a = list(col = ref.grids[1]),
                      b = list(col = ref.grids[2]),
                      c = list(col = ref.grids[3]))

    #add grids to plot
    extra.args$grids <- getPlotArgs("axis.line", extra.args$panel.scales, 
                                    local.resets = local.grids, user.resets = grids, 
                                    is.scales = TRUE, elements = c("a", "b", "c"))

    #add axes to plot
    extra.args$axes <- getPlotArgs("axis.line", extra.args$panel.scales, 
                                   local.resets = local.axis, user.resets = axes, 
                                   is.scales = TRUE, elements = c("a", "b", "c"))
    #add ticks to plot
    extra.args$ticks <- getPlotArgs("axis.line", extra.args$panel.scales, 
                                    local.resets = local.ticks, user.resets = ticks, 
                                    is.scales = TRUE, elements = c("a", "b", "c"))
    
    #add annotation to plot
    extra.args$annotation <- getPlotArgs("axis.text", extra.args$panel.scales, 
                                         local.resets = local.ann, user.resets = annotation, 
                                         is.scales = TRUE, elements = c("a", "b", "c"))

    #############
    #get z if set
    #############
     
    d1 <- try(latticeParseFormula(x, data, dimension = 3, 
                                  multiple = TRUE),
              silent = TRUE)
    if(is(d1)[1] == "try-error")
        stop("problem with z/data combination", call. = FALSE)

    my.z <- d1$left

    #and update names
    my.z.name <- d1$left.name 

    #################
    #possition in  code?
    #need to set these?
    ##################
    #this name could be '0' characters
    #could set zlab in call?
    #################
    
    ########
    #get a,b,c
    ########

    #a+b+c from x
    temp <- as.character(x)
    temp <- temp[length(temp)]
    x2 <- strsplit(temp,"[|]")[[1]]
    temp <- x2[1]
    temp <- strsplit(temp, "[+]")[[1]]

    #check enough there or error
    if(length(temp) < 3)
        stop("problem with 'a', 'b' and/or 'c' terms", call. = FALSE)

    temp <- as.formula(paste(temp[1], "~", temp[2], "*", temp[3], sep=""))    

    #get a,b,c from temp
    #and update names 
    d1 <- try(latticeParseFormula(temp, data, dimension = 3, 
                                  multiple = TRUE),
              silent = TRUE)
    if(is(d1)[1] == "try-error")
        stop("problem with x/data combination", call. = FALSE)

    #update a,b,c names
    #(note: these are taken from a ~ b * c) 
    if(is.null(extra.args$alab))
        extra.args$alab <- d1$left.name 
    if(is.null(extra.args$blab))
        extra.args$blab <- d1$right.x.name 
    if(is.null(extra.args$clab))
        extra.args$clab <- d1$right.y.name 

    #convert a,b,c to xy 
    ans <- triABC2XY(a=d1$left, b=d1$right.x, c=d1$right.y, 
                     ..., verbose=TRUE)
    ..my.x <- ans$x
    ..my.y <- ans$y

    #################################
    #NOTE:
    #################################
    #don't put a,b,c into xyplot
    #they match as multiple arguments
    #could put a,b,c in report in triABC2XY?
    #as a make safe/keep
    #################################

    ############
    #handle lims
    ############

    #get a/b/clims
    extra.args$alim <- ans$alim
    extra.args$blim <- ans$blim
    extra.args$clim <- ans$clim

    #get triangle xy dimensions
    #for x/ylims setting
    temp <- triABC2XY(a=c(ans$alim[1], ans$alim[1], ans$alim[2]), 
                      b=c(ans$blim[1], ans$blim[2], ans$blim[1]),
                      c=c(ans$clim[2], ans$clim[1], ans$clim[1]),  
                      verbose=FALSE)

####################################
#this next bit needs better handling
#needs to allow for label/no label
####################################

    #get lims
    extra.args$xlim <- range(temp$x, na.rm=TRUE)
    extra.args$ylim <- range(temp$y, na.rm=TRUE)

    #tidy lims
    temp <- function(lim, q1, q2){
                    if(diff(lim)==0) lim + q1 else
                                 lim + c(-(diff(lim)/q2[1]), (diff(lim)/q2[2])) 
    }
    if(is.null(extra.args$clab)){
        extra.args$xlim <- temp(extra.args$xlim, c(-0.5, 0.5), c(5,5))
        extra.args$ylim <- temp(extra.args$ylim, c(-0.5, 0.5), c(5,5))
    } else if(is.character(extra.args$clab) && extra.args$clab == ""){
            extra.args$xlim <- temp(extra.args$xlim, c(-0.5, 0.5), c(5,5))
            extra.args$ylim <- temp(extra.args$ylim, c(-0.5, 0.5), c(5,5))
        } else {
            extra.args$xlim <- temp(extra.args$xlim, c(-0.5, 0.5), c(5,5))
            extra.args$ylim <- temp(extra.args$ylim, c(-0.4, 0.5), c(4,5))
        }

    
    
    #make x xyplot formula
    #(put conditioning back in)
    x <- "..my.y~..my.x"
    if(length(x2)>1)
        x <- paste(x, x2[2], sep ="|")
    x <- as.formula(x)

    #############
    #z, col, cex handling
    #############

    extra.args <- listUpdate(list(z = my.z, ref = ..my.x), 
                             extra.args)

    extra.args$cex <- do.call(cexHandler, extra.args)

    extra.args <- listUpdate(extra.args, 
                             do.call(colHandler, extra.args))

    #############
    #scale up vectors
    #############
    extra.args <- listUpdate(extra.args, 
                      listExpand(extra.args, ignore=c("alim", "blim", "clim", "xlim", "ylim",
                                                      "alab", "blab", "clab", "panel.scales",
                                                      "at", "col.regions", "aspect", "ref", 
                                                      "a.grid", "b.grid", "c.grid",
                                                      "grids", "axes", "ticks", "annotation"), 
                          ref = extra.args$ref)
                  )

    #############
    #make and plot
    #############
    temp <- list(x=x, data=data, pch=20, aspect = 1, 
                 panel = function(..., subscripts){
                                  panel = panelPal(..., ignore=c("alim", "blim", "clim", "panel.scales", 
                                                   "grids", "axes", "ticks", "annotation",
                                                   "a.grid", "b.grid", "c.grid"), subscripts = subscripts, 
                                                    panel = function(..., user.panel = panel) 
                                                                panel.triangleFrame(..., user.panel = user.panel))
                         }
            )
    extra.args <- listUpdate(temp, extra.args)
    ans <- do.call(xyplot, extra.args)

    #any add-ins?
    ##possibly add the report?   
    ##ans$panel.args.common$whatever <- whatever

    #messy but means <-... still updates
    #might rethink
    plot(ans)
    invisible(ans) 

}



############################
############################
##panel...
############################
############################

########################
########################
##panel.triangleFrame
########################
########################


panel.triangleFrame <- function(x = NULL, y = NULL, a = NULL, b = NULL, c = NULL, 
                                ..., grids = grids, user.panel = panel.xyplot){

    #this currently assumes panel.scales output as from localScalesHandler

    #if x and y missing
    #try to make them using a, b and c
    extra.args <- list(...)

    if(is.null(x) & is.null(y)){

     #   a <- extra.args$a
     #   b <- extra.args$b
     #   c <- extra.args$c

        if(!is.null(a) & !is.null(b) & !is.null(c)){
            ans <- triABC2XY(a, b, c, ...)
            x <- ans$x
            y <- ans$y
        } else stop("missing x and y or a, b and c", call. = FALSE)
    }

    if(isGood4LOA(grids))
        panel.triangleGrids(..., grids = grids)
    panel.triangleAxes(...)
    user.panel(x=x, y=y, ...)

}

###########################
###########################
##panel.triangleAxes
###########################
###########################

#NOTES:

#axis temp is half way along axis, 
#             half way along and 0.1% perpendicular (to give offset)
#             then the min and max for the axis

#this might seem long winded 
#but allows use of any method of ABC -> XY scaling in 
#i.e. fixed scale/variable size (as in leic tri method) and variable scale/fixed size
#if ade4 method adopted then this could be simplified 
#        because all of these terms would be fixed but recovery of values would be harder




panel.triangleAxes <- function(alim = NULL, blim = NULL, clim = NULL, ..., 
                               panel.scales = panel.scales, axes = TRUE, 
                               ticks = TRUE, annotation = TRUE){

##############
##############
#rewrite for a,b,clab
#rethink panel.scales
##############
##############



    #extra.args
    extra.args <- list(...)

    #note currently the panel access is crude
    #if has to be in a, b or c elements of panel.scales, axes, ticks, annotation.
    #could add an axes id in panel.scale to handle this
    #or use getPlotArgs to check for general here?

    #prior 'correct' axis label placement and direction perpendicular to axis
    #looked silly
    #all:
    #ltext(x = temp$x[1] + (3*(temp$x[2] - temp$x[1])), y =temp$y[1] + (3*(y.offset = temp$y[2] - temp$y[1])),
    #      #[axis]#lab, adj = c(0.5,0.5))


    #make safe
    #to formals? 
    #or find better way to get a/b/clim
    if(is.null(alim)) alim <- c(0,1)
    if(is.null(blim)) blim <- c(0,1)
    if(is.null(clim)) clim <- c(0,1)

    alab <- if(is.null(extra.args$alab)) "" else extra.args$alab
    blab <- if(is.null(extra.args$blab)) "" else extra.args$blab
    clab <- if(is.null(extra.args$clab)) "" else extra.args$clab

    ##locate a point on an axis
    axis.loc <- function(n, lim)
                    (n * (max(lim, na.rm=TRUE) - min(lim, na.rm=TRUE))) + min(lim, na.rm=TRUE)
    
#######################
#rationalise next bit 
#calc done once
######################

    #a axis
    axes.pars <- getPlotArgs(default.as = "axis.line", source = panel.scales, elements = c("a", "b", "c"), 
                             is.scales=TRUE, user.resets = axes)  
    tick.pars <- getPlotArgs(default.as = "axis.line", source = panel.scales, elements = c("a", "b", "c"), 
                             is.scales=TRUE, user.resets = ticks)  
    ann.pars <- getPlotArgs(default.as = "axis.text", source = panel.scales, elements = c("a", "b", "c"), 
                             is.scales=TRUE, user.resets = annotation)  

    temp <- triABC2XY(c(axis.loc(0.5, alim), axis.loc(0.5, alim), axis.loc(0, alim), axis.loc(1, alim)),  
                      c(axis.loc(0.1, blim), axis.loc(0, blim), axis.loc(0, blim), axis.loc(0, blim)), 
                      c(axis.loc(0.4, clim), axis.loc(0.5, clim), axis.loc(1, clim), axis.loc(0, clim)))
    panel.localScale(panel.scale = panel.scales$a, x.loc = temp$x[3:4], y.loc = temp$y[3:4], lim = alim,
                     x.offset = temp$x[2] - temp$x[1], y.offset = temp$y[2] - temp$y[1], 
                     axis = axes.pars$a, ticks = tick.pars$a, annotation = ann.pars$a)

    #b axis
    temp <- triABC2XY(c(axis.loc(0.4, alim), axis.loc(0.5, alim), axis.loc(1, alim), axis.loc(0, alim)),
                      c(axis.loc(0.5, blim), axis.loc(0.5, blim), axis.loc(0, blim), axis.loc(1, blim)),  
                      c(axis.loc(0.1, clim), axis.loc(0, clim), axis.loc(0, clim), axis.loc(0, clim)))
    panel.localScale(panel.scale = panel.scales$b, x.loc = temp$x[3:4], y.loc = temp$y[3:4], lim = blim,
                     x.offset = temp$x[2] - temp$x[1], y.offset = temp$y[2] - temp$y[1], 
                     axis = axes.pars$b, ticks = tick.pars$b, annotation = ann.pars$b)


    #a,blabs
    #do a and b at same ht 'cos they look messy if you don't
    #likewise drop the standard perpendicular to axis cos one up/one down looks messy
    
    ltext(x = temp$x[1] - (3*(temp$x[2] - temp$x[1])) - (2 * (temp$x[1] - temp$x[3])), 
          y = temp$y[1] + (3*(y.offset = temp$y[2] - temp$y[1])),
          alab, adj = c(1,0.5))
    ltext(x = temp$x[1] + (3*(temp$x[2] - temp$x[1])), y = temp$y[1] + (3*(y.offset = temp$y[2] - temp$y[1])),
          blab, adj = c(0,0.5))

    #c axis 
    temp <- triABC2XY(c(axis.loc(0.1, alim), axis.loc(0, alim), axis.loc(0, alim), axis.loc(0, alim)),
                      c(axis.loc(0.4, blim), axis.loc(0.5, blim), axis.loc(1, blim), axis.loc(0, blim)),
                      c(axis.loc(0.5, clim), axis.loc(0.5, clim), axis.loc(0, clim), axis.loc(1, clim)))
    panel.localScale(panel.scale = panel.scales$c, x.loc = temp$x[3:4], y.loc = temp$y[3:4], lim = clim,
                     x.offset = temp$x[2] - temp$x[1], y.offset = temp$y[2] - temp$y[1], 
                     axis = axes.pars$c, ticks = tick.pars$c, annotation = ann.pars$c)


    #clab
    ltext(x = temp$x[2], y = temp$y[1] + (3*(y.offset = temp$y[2] - temp$y[1])), clab, adj = c(0.5, 0.5))


}


########################
########################
##panel.triangleGrids
########################
########################

#see notes in panel.triangleFrame

panel.triangleGrids <- function(a.grid = NULL, b.grid = NULL, c.grid = NULL, 
                                ..., alim = NULL, blim = NULL, clim = NULL,  
                                panel.scales = NULL, grids = TRUE){

    #to do
    #rationalise code 
    #rethink arguments/inputs

    #setup
    extra.args <- list(...)

    #make safe
    #to formals? better way?
    #also in ...Frame above
    if(is.null(alim)) alim <- c(0,1)
    if(is.null(blim)) blim <- c(0,1)
    if(is.null(clim)) clim <- c(0,1)

    #check for existing function in lattice?
    #could move into general list functions?
    getArg <- function(target = "at", source = NULL, element = "a", default = NULL){
                   if(is.null(source) || !is.list(source)) return(default)
                   if(!is.null(source[[element]][[target]])) return(source[[element]][[target]])
                   if(!is.null(source[[target]])) return(source[[target]])
                   default
    }

    #a axis

    #get safe settings
    if(is.null(a.grid))
        a.grid <- getArg("at", panel.scales, "a", NULL)
    if(is.null(a.grid))
        a.grid <- pretty(alim, getArg("tick.number", panel.scales, "a", 5))

    #confirm in range
    a.grid <- a.grid[a.grid<alim[2] & a.grid>alim[1]]

    #get grid pars
    grid.pars <- getPlotArgs(default.as = "axis.line", source = panel.scales, elements = "a", 
                             is.scales=TRUE, user.resets = grids)  

    temp <- function(x){
               a1 <- triABC2XY(c(x,x), c(blim[1], blim[2]-(x-alim[1])), c(clim[2]-(x-alim[1]), clim[1]))
                  if(isGood4LOA(grid.pars))
                      do.call(llines, listUpdate(list(x = a1$x, y = a1$y), grid.pars))
    }
    for(i in a.grid)
        temp(i)

    #b axis - as a but b

    if(is.null(b.grid))
        b.grid <- getArg("at", panel.scales, "b", NULL)
    if(is.null(b.grid))
        b.grid <- pretty(blim, getArg("tick.number", panel.scales, "b", 5))
    b.grid <- b.grid[b.grid<blim[2] & b.grid>blim[1]]
    grid.pars <- getPlotArgs(default.as = "axis.line", source = panel.scales, elements = "b", 
                             is.scales=TRUE, user.resets = grids) 
    temp <- function(x){
               b1 <- triABC2XY(c(alim[1], alim[2]-(x-blim[1])), c(x, x), c(clim[2]-(x-blim[1]), clim[1]))
                  if(isGood4LOA(grid.pars))
                      do.call(llines, listUpdate(list(x = b1$x, y = b1$y), grid.pars))
    }
    for(i in b.grid)
        temp(i)

    #c axis - as a but c

    if(is.null(c.grid))
        c.grid <- getArg("at", panel.scales, "c", NULL)
    if(is.null(c.grid))
        c.grid <- pretty(clim, getArg("tick.number", panel.scales, "c", 5))
    c.grid <- c.grid[c.grid<clim[2] & c.grid>clim[1]]
    grid.pars <- getPlotArgs(default.as = "axis.line", source = panel.scales, elements = "c", 
                             is.scales=TRUE, user.resets = grids)   
    temp <- function(x){
                c1 <- triABC2XY(c(alim[1], alim[2]-(x-clim[1])), c(blim[2]-(x-clim[1]), blim[1]), c(x, x))
                    if(isGood4LOA(grid.pars))
                        do.call(llines, listUpdate(list(x = c1$x, y = c1$y), grid.pars))
    }
    for(i in c.grid)
        temp(i)
}







#############################
#############################
##data handlers
#############################
#############################




##############################
##############################
##triABC2XY
##############################
##############################

triABC2XY <- function(a, b=NULL, c=NULL, ..., force.abc=TRUE, 
              if.na="remove.row", if.neg="remove.row", verbose=FALSE){

    #############
    #setup
    #############

    #extra.args
    extra.args <- list(...)

    #make a,b,c a data.frame

################
#could standardise this next bit and front end of triXY2ABC
#and make common function dataHandler
#could also put the logs in there
################

    data.abc <- if(is.data.frame(a)) a else
                    if(is.list(a)) as.data.frame(a) else
                       if(is.vector(a)) data.frame(a=a) else
                           stop("unable to handle supplied data", call. = FALSE)
###################
#possible issue if
#a shorter than b,c
###################
    if(is.vector(b))
        data.abc$b <- if(length(b) < nrow(data.abc))
                          rep(b, ceiling(nrow(data.abc)/length(b)))[1:nrow(data.abc)] else 
                              b[1:nrow(data.abc)]
    if(is.vector(c))
        data.abc$c <- if(length(c) < nrow(data.abc))
                          rep(c, ceiling(nrow(data.abc)/length(c)))[1:nrow(data.abc)] else 
                              c[1:nrow(data.abc)]
    #check dim
    if(ncol(data.abc) < 3)
        stop("insufficient data for 'abc' assignment", call. = FALSE)

    #force.abc/rescale
    temp <- data.abc[,1:3]
    if(force.abc){
        if("a" %in% names(data.abc)) temp[,1] <- data.abc$a
        if("b" %in% names(data.abc)) temp[,2] <- data.abc$b
        if("c" %in% names(data.abc)) temp[,3] <- data.abc$c
    } 
    data.abc <- temp
    abc.status <- rep(0, nrow(data.abc)) #abc.status

    ###########
    #if.neg and if.na
    ###########

##############
#need keep.as.is catcher
##############
    na.log <- apply(data.abc, 1, function(x) any(is.na(x))) #na
    neg.log <- apply(data.abc, 1, function(x) any(!is.na(x) & x<0)) #negs

    if(any(na.log)) {
        if(if.na == "remove.row")
            data.abc[na.log, 1:3] <- c(NA,NA,NA)
        if(if.na == "make.zero")
            data.abc[is.na(data.abc)] <- 0
    }

    if(any(neg.log)) {
        if(if.neg == "remove.row")
            data.abc[neg.log, 1:3] <- c(NA,NA,NA)
        if(if.neg == "make.zero")
            data.abc[data.abc<0] <- 0
        if(if.neg == "rescale.col")
            if(nrow(data.abc)==1)
                data.abc[data.abc<0] <- 0 else 
                    data.abc <- as.data.frame(apply(data.abc, 2, function(x) 
                        if(min(x, na.rm=TRUE)<0) x-min(x, na.rm=TRUE) else x))            
    }

#############
#below needs documenting in help
#############

    #abc 2 prop(abc)
    #function used again later
    prop.abc <- function(d){
        temp <- d[,1] + d[,2] + d[,3]
        d/temp
    }
    data.abc <- prop.abc(data.abc)

################################
################################

##############
#lim forcings
##############

    ##################
    #following based on ade4/Daniel Chessel method
    #################
    ##selection of sensible lim range

#could move this to separate/as data handlers
#

    #lim forcings
    if(is.null(extra.args$lims)){
        lims <- apply(data.abc, 2, range, na.rm = TRUE, finite = TRUE)
        lims[1,] <- (floor(lims[1,]/0.1))/10
        lims[2,] <- (floor(lims[2,]/0.1) + 1)/10
    } else {
              lims <- as.data.frame(matrix(rep(extra.args$lims[1:2], 3), 
                                    ncol = 3, nrow = 2))
    }
    if(!is.null(extra.args$alim))
        lims[,1] <- extra.args$alim
    if(!is.null(extra.args$blim))
        lims[,2] <- extra.args$blim
    if(!is.null(extra.args$clim))
        lims[,3] <- extra.args$clim
    if(!is.null(extra.args$abc.mins))
        lims[1,] <- extra.args$abc.mins
    if(!is.null(extra.args$abc.maxs))
        lims[2,] <- extra.args$abc.maxs

    #stop the bad
    lims[1,][lims[1,] < 0] <- 0
    lims[2,][lims[2,] > 1] <- 1

    #lims range
    #ade4 method

##############
#rethink this?
##############

    lim.lite <-function(lims){
        temp <- lims[2,] - lims[1,]
        temp2 <- max(temp)
        if (!all(temp == temp2)) {
            for (j in 1:3) {
                k <- temp2 - temp[j]
                while (k > 0) {
                    if ((k > 0) & (lims[2,j] < 1)) {
                        lims[2,j] <- lims[2,j] + 0.1
                        k <- k - 1
                    }
                    if ((k > 0) & (lims[1,j] > 0)) {
                        lims[1,j] <- lims[1,j] - 0.1
                        k <- k - 1
                    }
                }
            }
        }
        cal <- matrix(0, 9, 3)
        cal[1, 1] <- lims[1,1]
        cal[1, 2] <- lims[1,2]
        cal[1, 3] <- 1 - cal[1, 1] - cal[1, 2]
        cal[2, 1] <- lims[1,1]
        cal[2, 2] <- lims[2,2]
        cal[2, 3] <- 1 - cal[2, 1] - cal[2, 2]
        cal[3, 1] <- lims[2,1]
        cal[3, 2] <- lims[1,2]
        cal[3, 3] <- 1 - cal[3, 1] - cal[3, 2]
        cal[4, 1] <- lims[1,1]
        cal[4, 3] <- lims[1,3]
        cal[4, 2] <- 1 - cal[4, 1] - cal[4, 3]
        cal[5, 1] <- lims[1,1]
        cal[5, 3] <- lims[2,3]
        cal[5, 2] <- 1 - cal[5, 1] - cal[5, 3]
        cal[6, 1] <- lims[2,1]
        cal[6, 3] <- lims[1,3]
        cal[6, 2] <- 1 - cal[6, 1] - cal[6, 3]
        cal[7, 2] <- lims[1,2]
        cal[7, 3] <- lims[1,3]
        cal[7, 1] <- 1 - cal[7, 2] - cal[7, 3]
        cal[8, 2] <- lims[1,2]
        cal[8, 3] <- lims[2,3]
        cal[8, 1] <- 1 - cal[8, 2] - cal[8, 3]
        cal[9, 2] <- lims[2,2]
        cal[9, 3] <- lims[1,3]
        cal[9, 1] <- 1 - cal[9, 2] - cal[9, 3]
        lims[1,] <- apply(cal, 2, min)
        lims[1,] <- round(lims[1,], dig = 4)
        lims[2,] <- apply(cal, 2, max)
        lims[2,] <- round(lims[2,], dig = 4)
        #stop the bad
        lims[1,][lims[1,] < 0] <- 0
        lims[2,][lims[2,] > 1] <- 1
        lims
    }
    lims <- lim.lite(lims)

#check if next bit needed

    temp <- lims[2,] - lims[1,]
    temp2 <- max(temp)
    if (!all(temp == temp2))
          lims <- lim.lite(lims)
    if (!all(temp == temp2))
          lims <- lim.lite(lims)



################################
################################

    #check for out of range values
    ##compare with lims
    oor.log <- rep(FALSE, nrow(data.abc))
    oor.log <- ifelse(data.abc[,1] < min(lims[,1], na.rm=TRUE) |  
                      data.abc[,1] > max(lims[,1], na.rm=TRUE), TRUE, oor.log)
    oor.log <- ifelse(data.abc[,2] < min(lims[,2], na.rm=TRUE) |  
                      data.abc[,2] > max(lims[,2], na.rm=TRUE), TRUE, oor.log)
    oor.log <- ifelse(data.abc[,3] < min(lims[,3], na.rm=TRUE) |  
                      data.abc[,3] > max(lims[,3], na.rm=TRUE), TRUE, oor.log)
    data.abc[oor.log, 1:3] <- c(NA,NA,NA)

    #################
    ##following based on Mu Zhu, 
    ##Statistical Computing and Graphics 19,1, 2008
    #################
    #eta <- 0.08
    #anchor <- diag(3)
    #alpha1 <- c(1, -1, 0)/sqrt(2)
    #alpha2 <- c(-0.5, -0.5, 1)/sqrt(1.5)
    #vert.x <- anchor %*% alpha1
    #vert.y <- anchor %*% alpha2
 
    #x <- as.matrix(data.abc) * alpha1
    #y <- as.matrix(data.abc) * alpha2

    #ans <- list(x = x, 
    #            y = y,
    #            alim = lims[,1], blim = lims[,2], clim = lims[,3])

    ##################
    #following based on ade4/Daniel Chessel method
    #################
    ##with this abc -> xy scaling is local so triangle size remains constant
    ##if this is adopted we could simplify panel... functions
    ##rescale axis
    ##rescale.axis <- function(n, lim)
    ##                    (n - lim[1])/(lim[2]-lim[1])
    ##temp <- data.abc
    ##temp[,1] <- rescale.axis(temp[,1], lims[,1])
    ##temp[,2] <- rescale.axis(temp[,2], lims[,2])
    ##temp[,3] <- rescale.axis(temp[,3], lims[,3])
    ##temp <- prop.abc(temp)
    #basic return x,y a/b/clim
    ##ans <- list(x = (temp[,1] - temp[,3])/sqrt(2),
    ##            y = (2 * temp[,2] - temp[,1] - temp[,3])/sqrt(6),
    ##            alim = lims[,1], blim = lims[,2], clim = lims[,3])

    ##alternative method from Leic method?
    ##with this abc -> xy scaling fixed and triangle size changes
    ##with this panel... stay as is but triXY2ABC is greatly simplified
    ##also lim setting much less flexible
    
    ans <- list(x =  data.abc[,2]+(0.5*data.abc[,1]), 
                y = ((data.abc[,1]*0.866)*1.1)/1,      #confirm 1.1
                alim = lims[,1], blim = lims[,2], clim = lims[,3])

    if(!verbose) return(ans) 

    #full return
    #may want to rethink structure
    #re passing a,b,c to xyplot.formula...

    listUpdate(ans, list(a=data.abc[,1], b=data.abc[,2], c=data.abc[,3],
                         report = list(nas=na.log, negs=neg.log, oor=oor.log)))    
}



##############################
##############################
##triXY2ABC
##############################
##############################

triXY2ABC <- function(x, y=NULL, ..., force.xy=TRUE, verbose=FALSE){

    #############
    #setup
    #############
    #make xy a data.frame
    data.xy <- if(is.data.frame(x)) a else
                    if(is.list(x)) as.data.frame(x) else
                       if(is.vector(x)) data.frame(x=x) else
                           stop("unable to handle supplied data", call. = FALSE)
###################
#possible issue if
#x shorter than y - as above
###################

#need a lim checker/out of range handler
#need 

    if(is.vector(y))
        data.xy$y <- if(length(y) < nrow(data.xy))
                          rep(y, ceiling(nrow(data.xy)/length(y)))[1:nrow(data.xy)] else 
                              y[1:nrow(data.xy)]

    #check dim
    if(ncol(data.xy) < 2)
        stop("insufficient data for 'xy' assignment", call. = FALSE)

    #force.abc/rescale
    temp <- data.xy[,1:2]
    if(force.xy){
        if("x" %in% names(data.xy)) temp[,1] <- data.xy$x
        if("y" %in% names(data.xy)) temp[,2] <- data.xy$y
    } 
    data.xy <- temp

    a <- data.xy[,2]/(1.1 *0.866) * 1   #comfirm 1.1
    b <- (data.xy[,1] - (a * 0.5))
    c <- 1 - (a+b)

    ans <- list(a=a, b=b, c=c)

    if(!verbose) return(ans)    

    listUpdate(ans, list(x=data.xy[,1], y=data.xy[,2]))    
}



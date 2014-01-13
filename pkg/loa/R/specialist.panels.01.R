#in development code
#[TBC - NUMBER] functions 

#panel.kernelDensity
#panel.binPlot


#NOTE: much borrowed from lattice 

#to do
#callWithThis document or drop
#very minor


##############################
##############################
##panel.kernelDensity
##############################
##############################


panel.kernelDensity <- function (x, y, z = NULL, ..., 
          n = 20, kernel.fun = NULL, panel.range = TRUE, 
          process = TRUE, plot = TRUE, loa.settings = FALSE) 
{

    ####################
    #setup
    ####################

    extra.args <- list(...)


    if(!is.function(kernel.fun)){
        kernel.fun <- function(...) {
                          extra.args <- list(...)
                          ans <- do.call(MASS:::kde2d, extra.args)
                          output <- list(x = rep(ans$x, extra.args$n), y = rep(ans$y, 
                          each = extra.args$n), z = as.vector(ans$z))
                      }
        process.args <- unique(names(formals(MASS:::kde2d)))
    } else {
        process.args <- unique(names(formals(kernel.fun)))
    }

    plot.args <- unique(names(formals(panel.levelplot)))

    ###################
    #return safe.mode info
    ###################
    if(loa.settings)
        return(list(process.args = process.args, 
                    plot.args = plot.args,
                    group.args = c("col"),
                    default.settings = list(key.fun = "draw.loaColorRegionsKey", 
                                            isolate.col.regions = TRUE)))

    ###################
    #process section
    ###################

    if(process){

        temp <- length(x)
        mylist <- list(x = x, y = y, n = n)
        mylist <- listUpdate(mylist, extra.args, use = process.args)
 
        if (panel.range & !"lims" %in% names(extra.args)) {
            lims <- if("xlim" %in% names(extra.args) & "ylim" %in% names(extra.args))
                        list(xlim = extra.args$xlim, ylim = extra.args$ylim) else 
                        current.panel.limits()
            mylist$lims <- c(lims$xlim, lims$ylim)
        }
        kern.in <- do.call(kernel.fun, mylist)
        kern.in$z <- (kern.in$z/sum(kern.in$z)) * temp

    if(!plot) return(kern.in)
    } else {
        kern.in <- list(x=x, y=y, z=z)
    }

    ###########################
    #plot section
    ###########################

    if(plot){

        if (!"subscripts" %in% names(kern.in)) 
            kern.in$subscripts <- TRUE
        extra.args <- listUpdate(extra.args, kern.in)
        if (!"contour" %in% names(extra.args)) 
            extra.args$contour <- TRUE
        if (!"region" %in% names(extra.args)) 
            extra.args$region <- TRUE

        if (!"at" %in% names(extra.args)) 
            extra.args$at <- pretty(extra.args$zlim)

        temp <- length(extra.args$at)-1

#        extra.args$col.regions <- do.call(colHandler, 
#                                      listUpdate(extra.args, 
#                                          list(z=1:temp, ref=1:temp, zlim=c(1,temp))))

        extra.args$col.regions <- colHandler(1:(length(extra.args$at) - 1), col.regions = extra.args$col.regions, 
                                             alpha.regions = if(is.null(extra.args$alpha.regions)) extra.args$alpha else extra.args$alpha.regions,
                                             output = "col")

#colHandler(z=1:(length(temp)-1), 
#col.regions=extra.args$col.regions)

#order matters
#when removing alpha terms
# $alpha gets $alpha...

        extra.args$alpha.regions <- NULL


        if (!"col" %in% names(extra.args)) 
            extra.args$col <- trellis.par.get("dot.symbol")$col
        extra.args$col <- colHandler(1, col=extra.args$col, alpha=extra.args$alpha)

        extra.args$alpha <- NULL

        if("groups" %in% names(extra.args) || "zcases" %in% names(extra.args))
            do.call(groupsAndZcasesPanelHandler, listUpdate(extra.args, list(panel = panel.levelplot, plot=plot, process=process))) else
                do.call(panel.levelplot, extra.args)

     }

}



#####################################
#####################################
##panel.binPlot
#####################################
#####################################


panel.binPlot <- function(x = NULL, y = NULL, z = NULL, 
         breaks=20, x.breaks = breaks, y.breaks = breaks,
         x1=NULL, x2=NULL, y1=NULL, y2=NULL,
         statistic = mean, pad.grid = FALSE, ...,
         plot = TRUE, process = TRUE, loa.settings = FALSE 
         ){

#groups is somehow
#working #######how?

#tidy pass to lpolygon
##border to track par.settings
##reset from par.settings if present
##does par.settings want to be in ignore?

#pass statistic
##to work on
#make cuts flexible

##check which other args need to be common

#what does this do about dropped levels
#when cutting?

#lim which is not plot range?
#an option for cuts that fit to range

    if(loa.settings)
        return(list(group.args= c("col"),
                    zcase.args= c("pch"),
                    common.args = c("breaks", "pad.grid", "x.breaks", "y.breaks", "statistics"),
                    default.settings = list(key.fun = "draw.loaColorKey", x.elements = c("x", "x1", "x2"), 
                                            isolate.col.regions = TRUE)))

    extra.args <- list(...)
    
   


    #process
    if(process){    
        #x.bins
 
##this could be a function
##making a data.frame or list?
##then run again for y.bin

        temp <- if("xlim" %in% names(extra.args))
                    extra.args$xlim else range(x)
        x.cuts <- if(length(x.breaks)==1){
                      seq(min(temp), max(temp), length.out = (x.breaks + 1)) 
                  } else {
                      if(min(x.breaks) > min(temp)) temp <- c(min(temp), x.breaks)
                      if(min(x.breaks) < max(temp)) temp <- c(max(temp), x.breaks)
                      temp <- unique(sort(temp))
                  }
        x.case <- cut(x, x.cuts)
        x.1 <- x.cuts[-length(x.cuts)]
        x.2 <- x.cuts[-1]
        x.1.5 <- x.1 + ((x.2-x.1)/2)

        #y.bins
        temp <- if("ylim" %in% names(extra.args))
                    extra.args$ylim else range(y)
        y.cuts <- if(length(y.breaks)==1){
                      seq(min(temp), max(temp), length.out = (y.breaks + 1)) 
                  } else {
                      if(min(y.breaks) > min(temp)) temp <- c(min(temp), y.breaks)
                      if(min(y.breaks) < max(temp)) temp <- c(max(temp), y.breaks)
                      temp <- unique(sort(temp))
                  }
        y.case <- cut(y, y.cuts)
        y.1 <- y.cuts[-length(y.cuts)]
        y.2 <- y.cuts[-1]
        y.1.5 <- y.1 + ((y.2-y.1)/2)

        if(is.null(z)){
            #if no z's set:

            ##need a dummy set
            z <- rep(1, length=length(x))
            
            ##and only length is valid function
            statistic = length

            ##also should warning that this happened
            warning("no z values supplied; so binned as counts", call. = FALSE)                        
            
        }

        ans <- aggregate(z, data.frame(x.case,y.case), statistic)
        ans <- na.omit(ans)




#think about this
#

        temp <- ans$x.case
        levels(temp) <- x.1.5
        x <- as.numeric(as.character(temp))
        levels(temp) <- x.1
        x1 <- as.numeric(as.character(temp))
        levels(temp) <- x.2
        x2 <- as.numeric(as.character(temp))

        temp <- ans$y.case
        levels(temp) <- y.1.5
        y <- as.numeric(as.character(temp))
        levels(temp) <- y.1
        y1 <- as.numeric(as.character(temp))
        levels(temp) <- y.2
        y2 <- as.numeric(as.character(temp))

        z <- ans$x

#check position of pad grid for 

        #new bit re pad.grid
        if(pad.grid){
            
            #add in the NA cases if padded grid out fully
            #might replace this with a drop or na.action option later?

            test <- expand.grid(list(x=x.1.5, y=y.1.5))
            test <- cbind(test, expand.grid(list(x1=x.1, y1=y.1)))
            test <- cbind(test, expand.grid(list(x2=x.2, y2=y.2)))

            ans <- data.frame(x=x, y=y,z=z)

            ans <- merge(test, ans, all=TRUE)

            x <- ans$x
            y <- ans$y
            z <- ans$z
            x1 <- ans$x1
            y1 <- ans$y1
            x2 <- ans$x2
            y2 <- ans$y2            

        }


#think about x1, x2, y1, y2
#do we need them?

        if(!plot)
            return(list(x=x, y=y, z=z, x1=x1, x2=x2, y1=y1, y2=y2))
    }

    #plot
    if(plot){

######################
#new bit
######################

#       temp <- listUpdate(extra.args, list(x=x, y=y, z=z, subscripts=1:length(x)))
#       if(!"at" %in% names(temp))
#           temp$at <- seq(min(temp$zlim), max(temp$zlim), length.out = 100)


#       do.call(panel.levelplot, temp)

#################

############################
#replaced bit
############################

##warning if groups or zcases present
##and strip out before passing on

        if(!"at" %in% names(extra.args))
            extra.args$at <- seq(min(extra.args$zlim), max(extra.args$zlim), length.out=100)

#if not given
#col in boxes from col.regions

        col <- do.call(colHandler, listUpdate(extra.args, list(z=z, ref=z), ignore="col"))

#border handling 
#need to tidy this

#        if(is.null(border)){
#           if("col" %in% names(extra.args))
#               border <- extra.args$col[1] else 
#                  border <- FALSE
#        }


#link into fault settings
#scheme, etc, see panel.polar... examples


#this gets col/alpha setting
#might need to rethink alpha/alpha.regions handling in colHander

        if(is.null(extra.args$border) && "col" %in% names(extra.args))
            extra.args$border <- do.call(colHandler, 
                                         listUpdate(extra.args, list(z=1, ref=1, zlim=c(1,1)),  
                                         ignore=c("col.regions", "alpha.regions")))

        if(is.null(extra.args$border))
            extra.args$border <- FALSE

#think about passing lty, etc. 
#for border line properties 
        
        lrect(x1, y1, x2, y2, col=col, border=extra.args$border )#, alpha=extra.args$alpha)


#        for(i in 1:length(x1)){

##this could be all panel.elements
##

#            temp <- list(x = c(x1[i], x1[i], x2[i], x2[i]), 
#                         y = c(y1[i], y2[i], y2[i], y1[i]),
#                         col = extra.args$col[i])
#            temp <- listUpdate(extra.args[!names(extra.args) %in% c("x1", "x2", "y1", "y2")], temp)

#might not need all this
#track border

#            temp <- listUpdate(extra.args, temp)
#            do.call(lpolygon, temp)
#        }


##################################

    }
}

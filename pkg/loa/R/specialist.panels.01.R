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
                    plot.args = plot.args))

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
            extra.args$region <- FALSE

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
         statistic = mean, ...,
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
                    common.args = c("breaks", "x.breaks", "y.breaks", "statistics"),
                    default.settings = list(key.fun = "draw.loaColorKey")))

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

        if(!plot)
            return(list(x=x, y=y, z=z, x1=x1, x2=x2, y1=y1, y2=y2))
    }

    #plot
    if(plot){

##warning if groups or zcases present
##and strip out before passing on

        if(!"at" %in% names(extra.args))
            extra.args$at <- seq(min(extra.args$zlim), max(extra.args$zlim), length.out=100)
        extra.args$col <- do.call(colHandler, listUpdate(list(z=z), extra.args))
        for(i in 1:length(x1)){

##this could be all panel.elements
##

            temp <- list(x = c(x1[i], x1[i], x2[i], x2[i]), 
                         y = c(y1[i], y2[i], y2[i], y1[i]),
                         col = extra.args$col[i])
            temp <- listUpdate(extra.args[!names(extra.args) %in% c("x1", "x2", "y1", "y2")], temp)

#might not need all this
#track border

            temp <- listUpdate(extra.args, temp)
            do.call(lpolygon, temp)
        }
    }
}



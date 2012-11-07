#in development code
#[TBC - NUMBER] functions 

#panel.kernelDensity


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

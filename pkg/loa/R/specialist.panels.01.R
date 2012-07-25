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


panel.kernelDensity <- function (x, y, z = NULL, ..., n = 20, 
                                 kernel.fun = NULL, panel.range = TRUE) 
{
    extra.args <- list(...)
    temp <- length(x)
    mylist <- list(x = x, y = y, n = n)
    allowed <- unique(names(formals(MASS:::kde2d)))
    mylist <- listUpdate(mylist, extra.args, use = allowed)
    if (panel.range & !"lims" %in% names(extra.args)) {
        lims <- current.panel.limits()
        mylist$lims <- c(lims$xlim, lims$ylim)
    }
    kernel01 <- function(...) {
        extra.args <- list(...)
        ans <- do.call(MASS:::kde2d, mylist)
        output <- list(x = rep(ans$x, extra.args$n), y = rep(ans$y, 
            each = extra.args$n), z = as.vector(ans$z))
    }
    if (is.null(kernel.fun)) 
        kernel.fun <- kernel01
    kern.in <- do.call(kernel01, mylist)
    kern.in$z <- (kern.in$z/sum(kern.in$z)) * temp
    if (!"subscripts" %in% names(kern.in)) 
        kern.in$subscripts <- TRUE
    extra.args <- listUpdate(extra.args, kern.in)
    if (!"at" %in% names(extra.args)) 
        extra.args$at <- pretty(extra.args$z, 10)
    if (!"contour" %in% names(extra.args)) 
        extra.args$contour <- TRUE
    if (!"region" %in% names(extra.args)) 
        extra.args$region <- FALSE
    do.call(panel.contourplot, extra.args)
}

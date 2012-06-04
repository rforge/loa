#in development code
#[TBC - NUMBER] functions 

#getXY
#getLatLon

#NOTE: much borrowed from lattice 


#####################
#####################
##getXY
#####################
#####################

#locate points on plot
#with correction for 
#[in development]

#to do

#think about getXY argument order
#think about getLatLon order
#might want to hide unit 
#drop plot.list control?

#overplotting issue with using alpha
#see e.g. getXY(n=5, col="black", cex=10, alpha=0.5)


getXY <- function (n = -1, ..., unit = "native", scale.correction = NULL) 
{

    #check
    if(n == 0)
        stop("'n = 0' ends call without action. (See ?getXY)", call. = FALSE)

    #setup
    extra.args <- list(...)
    ans <- NULL
    n <- n - 1

    #get safe trellis focus
    focus.list <- listUpdate(extra.args, list(), 
                             use.a = names(formals(trellis.focus)))   
    do.call(trellis.focus, focus.list)

    #check for plot arguments
    plot.list <- listUpdate(extra.args, list(), 
                             use.a = c("type", "pch", "lty", "col", 
                                          "cex", "lwd", "font", "fontfamily", 
                                          "fontface", "col.line", "col.symbol", 
                                          "alpha", "fill"))

#####################
#this loop could be tidier
#something better than while?
#####################

    #loop grid.locator 
    while (!is.null(temp <- grid.locator(unit = unit)) & n != 0) {
        n <- n - 1
        ans$x <- c(ans$x, as.numeric(temp$x))
        ans$y <- c(ans$y, as.numeric(temp$y))
        if(length(plot.list)>0)
            lpoints(ans$x, ans$y, ...)
    }
    #check last case
    if(!is.null(temp)){
        ans$x <- c(ans$x, as.numeric(temp$x))
        ans$y <- c(ans$y, as.numeric(temp$y))
        if(length(plot.list)>0)
            lpoints(ans$x, ans$y, ...)
    }

    #rescale if required

############
#could be make this 
#function only
############

    if (!is.null(scale.correction)) 
        ans <- scale.correction(ans)

    #unfocus and return results
    trellis.unfocus()
    ans
}



#####################
#####################
##getLatLon
#####################
#####################

getLatLon <- function(..., map = NULL, object = trellis.last.object(),  
            scale.correction = function(x) {
                                temp <- XY2LatLon(map, x$x, x$y)
                                as.list(as.data.frame(temp))
                            })
{

    if(is.null(map))
        map <- getMapArg(object)

#need new error catcher

#    if(missing(map))
#        stop("need 'map' as reference. (See ?getLatLon)", 
#             call. = FALSE)

    #pass to getXY 
    getXY(..., scale.correction = scale.correction)
}




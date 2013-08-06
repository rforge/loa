
panel.oldzcasePieSegmentPlot <- function(..., zcase.rescale=TRUE, 
                       loa.settings = FALSE
){


#panel.zcasePieSegementPlot?
#set col as zcase args
#need to make handler for zcase args that are not declared
#    as above?
#    in stepwise handler
#need to sort out z to radius behaviour
#    maybe use cexHandler?

#urgent

#think about arg names 
#especially normalise.zcase 

#think about defaults
#e.g. key and borders

#think about names

    if (loa.settings) 
        return(list(zcase.args = c("col"), 
            default.settings = list(key = FALSE, grid = FALSE)))

    temp.glyph <- function(..., zcase.ref = NULL){

        extra.args <- list(...)

        extra.args$angle <- 360/zcase.ref[2]
        extra.args$start <- extra.args$angle * (zcase.ref[1]-1)

        #this bit is to center first segment on 0deg
        #could turn it off and on

        extra.args$start <- extra.args$start - (extra.args$angle/2)
        if(!"radius" %in% names(extra.args)){
            temp <- extra.args
            if(zcase.rescale){
                 if("zcase.zlim" %in% names(extra.args))
                      temp$zlim <- extra.args$zcase.zlim[[zcase.ref[1]]]              
            }
            extra.args$radius <- do.call(cexHandler, temp)
        }
        do.call(loaPieSegment, extra.args)
    }

    stepwiseZcasesGlyphHandler(..., loaGlyph = temp.glyph)

}






test <- function(...){
    xyplot(..., panel = function(x, y, x.loc, y.loc,...){
        panel.xyplot(x=x, y=y, ...)
        panel.localScale(x=x.loc, y=y.loc, ...)
    })
}


test(0:10~0:10, panel.scale=list(), x.loc=c(1,10), y.loc=c(1,1), lim=c(1,10))





dataHandler <- function(..., force = TRUE, elements = c("x", "y")){



#####################################working


##############
    #isGood4Loa on elements or ...?
##############

    #############
    #setup
    #############

    #extra.args
    extra.args <- list(...)

    #make a data.frame

###############
    #list not working below
###############


    my.data <- if(is.data.frame(extra.args[[1]])) extra.args[[1]] else
                   if(is.list(extra.args[[1]])) listExpand(extra.args[[1]], TRUE, ref = extra.args[[1]][[1]]) else
                      if(is.vector(extra.args[[1]])) data.frame(a=extra.args[[1]]) else
                          stop("unable to handle supplied data", call. = FALSE)


################this far


###################
#possible issue if
#a shorter than b,c
###################


return(my.data)

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

}






####################################
####################################





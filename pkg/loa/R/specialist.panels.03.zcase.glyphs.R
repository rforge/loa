#in development code
#[TBC - NUMBER] functions 

#panel.zcasePieSegmentPlot

#NOTE: much borrowed from... 

#to do

############################
#repairs
############################

#



##############################
##############################
##panel.zcasePieSegmentPlot
##############################
##############################




panel.zcasePieSegmentPlot <- function(..., zcase.rescale=TRUE, 
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




#loaPlot(a+b~a*b, panel=panel.zcaseSegmentPlot)

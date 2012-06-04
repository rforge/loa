#in development code
#[TBC - NUMBER] functions 

#localScalesHandler
#panel.localScale


######################
#to do
######################
#tidy the code and add remarks
####
#think about panel.localScale defaults
#could be more sensible
####
#

#NOTE: much borrowed from lattice 


##############################
##############################
##localScalesHandler
##############################
##############################


#this needs a lot of tidying

localScalesHandler <- function(scales = NULL, ..., allowed.scales =c("x", "y"), 
                             disallowed.scales = NULL, remove.box = FALSE){

    ####################
    #scales handling for non-xy systems
    ####################

    extra.args <- list(...)

    #remove any disallowed.scales
    if(!is.null(disallowed.scales) && is.character(disallowed.scales)){
        if(!is.null(scales))
            scales <- scales[names(scales)[!names(scales) %in% disallowed.scales]]    
    }

##############
#might need to rethink this?
#is there a better source for temp?
#stable means of give more scales control?
##############

    temp <- list(draw = TRUE, arrows = FALSE, tick.number = 5, abbreviate = FALSE,
                 minlength = 4, tck = 1)
    temp2 <-  scales[names(scales)[!names(scales) %in% allowed.scales]]

######################
#need to link in parameters and extras
#e.g. if at set
######################

    def.scales <- listUpdate(temp, temp2)

    scales <- scales[names(scales)[names(scales) %in% allowed.scales]]

    #could introduce new.scales
    #so output just a,b,c?
    for(i in allowed.scales){
        scales[[i]] <- if(is.null(scales[[i]]))
                           def.scales else listUpdate(def.scales, scales[[i]])
    }

    #return relevant components
    output <- list(scales = list(draw=FALSE),
                   panel.scales = scales)
    

#############
#might to harder code below
#as someone could call par.settings
#############

    if(remove.box){
        #par.setting for no box
        temp <- list(axis.line = list(col = "transparent"))
        output$par.settings <- if(is.null(extra.args$par.settings))
                                   temp else listUpdate(temp, extra.args$par.settings)
    }    

    output

}






##############################
##############################
##panel.localScale
##############################
##############################

#problems
##################
#scale line black by default?
#or axis default?
##################
#label text cex = 0.8?
###################

#to dos
#################
#if no elements in at, labels,
#list handler at end
#arrows handler
#abbrev handler 
#label.before handler
#plot at range, tick.range=FALSE?
################

#to think about
##################
#defaults for this 
#locs and lim?
##################

panel.localScale <- function(x.loc, y.loc, lim, ..., 
                        panel.scale = NULL, label.before = TRUE, x.offset = NULL, 
                        y.offset = NULL, axis = TRUE, ticks = TRUE, annotation = TRUE){

    #setup
    extra.args <- list(...)

#could move next into getPlotArgs
#or make a getScaleArgs

    #update panel.scale
    if(is.null(panel.scale)) panel.scale <- list()
    panel.scale <- listUpdate(list(draw = TRUE, arrows = FALSE, 
                                   tick.number = 5, abbreviate = FALSE, 
                                   minlength = 4, tck = 1, col.line = 1, 
                                   cex = 0.8), panel.scale)

    #get local scaled x,y ranges 
    ##might want to rethink
    ##might if this out if not offsets
    ##might also make this a function
    x.loc <- as.numeric(grid:::convertX(grid:::unit(x.loc, "native"), "npc"))
    y.loc <- as.numeric(grid:::convertY(grid:::unit(y.loc, "native"), "npc"))

    #x and y vectors
    x.v <- if(x.loc[1]==x.loc[2])
               0 else (x.loc[2]-x.loc[1])/(lim[2]-lim[1]) 
    y.v <- if(y.loc[1]==y.loc[2])
               0 else (y.loc[2]-y.loc[1])/(lim[2]-lim[1])    
 
    #at and labels
    at <- if(is.null(panel.scale$at))
              pretty(lim, panel.scale$tick.number) else panel.scale$at

    #note: labels can be list
    labels <- if(is.null(panel.scale$labels))
                  at else if(is.list(panel.scale$labels))
                      if(is.null(panel.scale$labels$labels))
                          at else panel.scale$labels$labels 
                      else panel.scale$labels

    #remove out of range at and labels
    labels <- labels[at >= lim[1] & at <= lim[2]]
    at <- at[at >= lim[1] & at <= lim[2]]

    ##############
    #lot of stuff 
    #only needed if ticks are there
    ##############

    if(length(at)>0){

        corr <- 1/sqrt((x.v^2)+(y.v^2))
        temp <- (at-lim[1])

        my.x <- x.loc[1] + (temp * x.v)
        my.y <- y.loc[1] + (temp * y.v)

        if(is.null(x.offset)){
            my.x2 <- my.x - (0.025 * panel.scale$tck * y.v * corr)
            my.x3 <- my.x - (0.05 * panel.scale$tck * y.v * corr)
        }    
        if(is.null(y.offset)){
            my.y2 <- my.y + (0.025 * panel.scale$tck * x.v * corr)
            my.y3 <- my.y + (0.05 * panel.scale$tck * x.v * corr)
        }

        my.x <- as.numeric(grid:::convertX(grid:::unit(my.x, "npc"), "native"))

        if(is.null(x.offset)){
            my.x2 <- as.numeric(grid:::convertX(grid:::unit(my.x2, "npc"), "native"))
            my.x3 <- as.numeric(grid:::convertX(grid:::unit(my.x3, "npc"), "native"))
        } else {
            my.x2 <- my.x + (0.5 * x.offset)
            my.x3 <- my.x + x.offset
        }

        my.y <- as.numeric(grid:::convertY(grid:::unit(my.y, "npc"), "native"))

        if(is.null(y.offset)){
            my.y2 <- as.numeric(grid:::convertY(grid:::unit(my.y2, "npc"), "native"))
            my.y3 <- as.numeric(grid:::convertY(grid:::unit(my.y3, "npc"), "native"))
        } else {
            my.y2 <- my.y + (0.5 * y.offset)
            my.y3 <- my.y + y.offset
        }

    } #end of tick maker

    x.loc <- as.numeric(grid:::convertX(grid:::unit(x.loc, "npc"), "native"))
    y.loc <- as.numeric(grid:::convertY(grid:::unit(y.loc, "npc"), "native"))

    #get panel.scale bits for line
    axis.pars <- getPlotArgs("axis.line", panel.scale, is.scales=TRUE, user.resets = axis)
    if(isGood4LOA(axis.pars))
        do.call(llines, listUpdate(list(x=x.loc, y= y.loc), axis.pars))

    #get ticks and annotation and add
    ticks.pars <- getPlotArgs("axis.line", panel.scale, is.scales=TRUE, user.resets = ticks)
    annotation.pars <- getPlotArgs("axis.text", panel.scale, is.scales=TRUE, user.resets = annotation)

    if(length(at)>0){
        for(i in 1:length(at))
            if(isGood4LOA(ticks.pars))
                do.call(llines, listUpdate(list(x = c(my.x[i], my.x2[i]), 
                                                y = c(my.y[i], my.y2[i])), ticks.pars))

        if(isGood4LOA(annotation.pars))
            do.call(ltext, listUpdate(list(x = my.x3, y = my.y3,
                                           labels = labels), annotation.pars)) 
    }       

}






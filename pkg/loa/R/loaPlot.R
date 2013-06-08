#in development code
#[1 -TBC] functions 


#loaPlot - main function
#panel.loaPlot
#panel.loaPlot2


#######################
##might want to make 
##own space for conds
#######################


#formulaHandler - handles the x formula

#urgent
#fixes 
#to dos
#suggestions


###########################
###########################
#loaPlot
###########################
###########################


loaPlot <- function (x, data = NULL, panel = panel.loaPlot, ..., 
     local.scales = FALSE, reset.xylims = TRUE, load.lists = NULL,
     by.group = NULL, by.zcase = NULL, preprocess = TRUE) 
{

#and adding loa.settings = NULL
#to allow user reset of loa.settings

#then need the same in panelPal 

#also need to update panelPal for 
#group process and group plot
#need to think about 
#panel.loaPlotGroups

    ###################################
    #set up
    ###################################

    #general
    extra.args <- list(...)


    #from lattice
    ##groups <- eval(substitute(groups), data, environment(x))
    #env <- environment(x)

    #check for any panel defaults
    loa.settings <- loaHandler(panel)

##########################
##########################
##this bit could be tidier
##########################
##########################

    group.args <- if(is.null(extra.args$group.args))
                      NULL else extra.args$group.args

    if(is.list(loa.settings)){

        temp <- loa.settings$default.settings

        if(is.list(temp))

##think about because this is not great

            if("local.scales" %in% names(temp)){
                local.scales <- temp$local.scales
            }
            if("reset.xylims" %in% names(temp)){
                reset.xylims <- temp$reset.xylims
            }
            if("load.lists" %in% names(temp)){
                load.lists <- temp$load.lists
            }
            extra.args <- listUpdate(temp, extra.args)

#check next bit 
#might not be needed any more
        if(is.null(group.args))
            group.args <- loa.settings$group.args

    }

    #list.loads
    if(is.character(load.lists)){
        for(i in load.lists)
            extra.args <- do.call(listLoad, listUpdate(extra.args, list(load = i)))
    }

############################
#new addition
#par.settings handling
############################
   
   #par.settings 
   extra.args <- do.call(listLoad, listUpdate(extra.args, list(load="par.settings")))
   extra.args$par.settings <- do.call(parHandler, extra.args)

#


#print(extra.args)

    reset.aspect = FALSE
    if(!is.null(extra.args$aspect)){
        if(is.character(extra.args$aspect) && extra.args$aspect == "loa.iso"){
            extra.args$aspect <- 1
            reset.aspect <- TRUE
        }

    }
        


    ###################################
    #key test
    ###################################
    legend <- do.call(keyHandler, listUpdate(extra.args, list(output = "legend")))
    extra.args <- do.call(keyHandler, listUpdate(extra.args, list(output = "other.args")))

    ###################################
    #local scales
    ###################################
    if(local.scales){

#could strip out the localscaleshandler args 
#after we run this?

        temp <- listUpdate(list(remove.box = TRUE), extra.args)
        extra.args <- listUpdate(extra.args, do.call(localScalesHandler, temp))

#print((extra.args$panel.scales))

        extra.args$xlab = ""
        extra.args$ylab = "" 
    }

    ###################################
    #main routine
    ###################################

    temp <- listUpdate(list(x=x, data = data),
                       extra.args)

    d1 <- do.call(formulaHandler, temp)

#return(d1)

##################################
#could this go into formulaHandler?
##################################

#temp fix for conditioning labels
    extra.args <- do.call(stripHandler,
                          listUpdate(list(striplab = names(d1$panel.condition)), extra.args)
                         )


    ..loa.x <- d1$x
    ..loa.y <- d1$y

     extra.args$z <- d1$z
     extra.args$ref <- d1$x
     extra.args <- listUpdate(list(xlab = d1$x.name, ylab = d1$y.name, zlab = if(is.null(extra.args$z)) NULL else d1$z.name),
                              extra.args)

    if("zcases" %in% names(d1))
        extra.args$zcases <- d1$zcases


    x <- "..loa.y~..loa.x"
    if(!is.null(d1$panel.condition) && length(d1$panel.condition)>0){
        ..loa.cond <- d1$panel.condition
        temp <- paste("..loa.cond[[" , 1:length(..loa.cond), sep="")
        temp <- paste(temp, "]]", sep="", collapse="+")
        x <- paste(x, temp, sep="|")

    }
#         ..loa.for <- paste(..loa.for, d1$panel.condition, sep ="|")

    
    extra.args$x <- as.formula(x)
    extra.args$panel <- function(..., subscripts) panel.xyplot(..., subscripts=subscripts)
              
    ans <- do.call(xyplot, extra.args)

    ans <- panelPal(ans, panel=panel, preprocess = preprocess,
                    by.group = by.group, by.zcase = by.zcase, 
                    reset.xylims = reset.xylims, legend = legend)


    if(reset.aspect){
        temp <- (max(ans$y.limits) - min(ans$y.limits))/
                (max(ans$x.limits) - min(ans$x.limits))
        ans$aspect.ratio <- temp
    }


################
#tidy this bit
#later
################

    #handle pch
#    temp <- unique(unlist(lapply(ans$panel.args, names)))

#    temp <- unique(c(names(ans$panel.args.common), temp))


#    if(!"pch" %in% temp)
#        ans$panel.args.common$pch <- pchHandler()

                                            
#############################
#use GoogleMap output method?
#check output reports?
#############################

    ans

}






############################
############################
##panel.loaPlot2
############################
############################

################
#working on this
#################
#issue with condPanelHandler
#so reset from ...loaPlot to ...loaPlot2
#################


panel.loaPlot2 <- function(..., loa.settings = FALSE){

#################
#panel to link 
#cex and col to 
#colorkey and z
#################

#think about
##################
#update from list(output="col") for colHandler?
#to make more robust


    ###################
    #return safe.mode info
    ###################
    if(loa.settings)
        return(list(group.args= c("col"),
                    zcase.args= c("pch"),
                    default.settings = list(key.fun = "draw.loaPlotZKey")))



    plot.fun <- function(...){
                    extra.args <- list(...)
#                    if(length(extra.args$x)>0 & length(extra.args$y)>0){
                        extra.args$col <- do.call(colHandler, extra.args)
                        extra.args$cex <- do.call(cexHandler, extra.args)
                        extra.args$pch <- do.call(pchHandler, listUpdate(extra.args, list(z=NULL)))
                        do.call(panel.xyplot, extra.args)
#                    } else print("HO")
                }
    do.call(groupsAndZcasesPanelHandler, listUpdate(list(...), list(panel=plot.fun)))
}




############################
############################
##panel.loaPlot
############################
############################

################
#working on other version
#################
#issue with condPanelHandler
#so reset from ...loaPlot2 to ...loaPlot
#this now default
#################


panel.loaPlot <- function(..., loa.settings = FALSE){

#################
#panel to link 
#cex and col to 
#colorkey and z
#################

#think about
##################
#update from list(output="col") for colHandler?
#to make more robust


    ###################
    #return safe.mode info
    ###################
    if(loa.settings)
        return(list(group.args= c("col"),
                    zcase.args= c("pch"),
                    default.settings = list(key.fun = "draw.loaPlotZKey")))

    extra.args <- list(...)

    if("groups" %in% names(extra.args)){
        if("group.args" %in% names(extra.args) && length(extra.args$group.args)>0){

#group.ids might not always bee there

            temp <- as.numeric(factor(extra.args$groups, levels = extra.args$group.ids))
            for(i in extra.args$group.args){
                extra.args[[i]] <- extra.args[[i]][temp]
            }
        }
        extra.args$groups <- NULL
    }

    if("zcases" %in% names(extra.args)){
        if("zcase.args" %in% names(extra.args) && length(extra.args$zcase.args)>0){

#zcase.ids might not always bee there

            temp <- as.numeric(factor(extra.args$zcases, levels = extra.args$zcase.ids))
            for(i in extra.args$zcase.args){
                extra.args[[i]] <- extra.args[[i]][temp]
            }
        }
        extra.args$zcases <- NULL
    }

    extra.args$col <- do.call(colHandler, extra.args)
    extra.args$cex <- do.call(cexHandler, extra.args)
    extra.args$pch <- do.call(pchHandler, listUpdate(extra.args, list(z=NULL)))
 
    do.call(panel.xyplot, extra.args)
 }






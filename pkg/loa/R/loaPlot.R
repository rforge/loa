#in development code
#[1 -TBC] functions 


#loaPlot - main function
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


loaPlot <- function (x, data = NULL, panel = panel.xyplot, 
    ..., safe.mode = TRUE, preprocess = TRUE, reset.xylims = TRUE) 
{

    ###################################
    #set up
    ###################################

    ocall <- sys.call(sys.parent())
    ocall[[1]] <- quote(test)
    ccall <- match.call()
    extra.args <- list(...)

    ###################################
    #main routine
    ###################################

    ccall$data <- data
    ccall$panel <- function(..., subscripts) panel.xyplot(..., subscripta=subscripts)

    temp <- listUpdate(list(x=x, data=data), extra.args)
    d1 <- do.call(formulaHandler, temp)

##################################
#could this go into formulaHandler?
##################################

    ..loa.x <- d1$x
    ..loa.y <- d1$y

     extra.args$z <- d1$z
     extra.args$ref <- d1$x
     extra.args <- listUpdate(list(xlab = d1$x.name, ylab = d1$y.name, zlab = d1$z.name),
                              extra.args)

    if("z.condition" %in% names(d1))
        extra.args$z.condition <- d1$z.condition

    x <- "..loa.y~..loa.x"
    if(!is.null(d1$panel.condition))
         x <- paste(x, d1$panel.condition, sep ="|")
    ccall$x <- as.formula(x)

    extra.args <- listUpdate(list(pch=20), extra.args)

    ccall[names(extra.args)] <- extra.args

    ccall[[1]] <- quote(lattice::xyplot)
    ans <- eval(ccall, parent.frame())

    ans <- panelPal2(ans, panel=panel, preprocess = preprocess,
                     safe.mode = safe.mode, reset.xylims = reset.xylims)

#############################
#use GoogleMap output method?
#check output reports?
#############################

    ans$call <- ocall
    ans
}






###########################
###########################
#formulaHandler
###########################
###########################

formulaHandler <- function(x, data = NULL, ..., check.xy.dimensions=TRUE){

    #extra.args
    extra.args <- list(...)

    if(!"loa.err.message" %in% names(extra.args))
        extra.args$loa.err.message <- "problem with x/data combination"

    #get base equation and z terms

    allowed.args <- names(formals(latticeParseFormula))
    temp <- listUpdate(list(dimension=3, multiple=TRUE, outer=TRUE), 
                             extra.args, use.b=allowed.args)
    temp <- temp[names(temp) != "model"]
    temp <- listUpdate(list(model=x, data=data), temp)

    d1 <- try(do.call(latticeParseFormula, temp), silent = TRUE)
    if(is(d1)[1] == "try-error")
        stop(extra.args$loa.err.message, call. = FALSE)

    #the end conditioning
    temp <- as.character(x)
    temp <- temp[length(temp)]
    cond <- strsplit(temp, "[|]")[[1]]
    if(length(cond)>1)
        d1$panel.condition <- cond[2]    


####################
#tidy these
####################

    #the z, z condition
    names(d1)[names(d1)=="left"] <- "z"
    names(d1)[names(d1)=="left.name"] <- "z.name"
    z <- d1$left
    if(length(d1$condition)>0){
        if(is.null(names(d1$condition)))
            names(d1)[names(d1)=="condition"] <- "z.condition" else
                if(length(d1$condition) > length(names(d1$condition)[names(d1$condition)!=""]))
                    d1$z.condition <- d1$condition[names(d1$condition)==""]
    }

    #x,y
    names(d1)[names(d1)=="right.x"] <- "x"
    names(d1)[names(d1)=="right.x.name"] <- "x.name"
    names(d1)[names(d1)=="right.y"] <- "y"
    names(d1)[names(d1)=="right.y.name"] <- "y.name"

#######################
#check for extra dimensions in x, y
#currently x or y not both!
#if allowed?
#######################
#may rethink this

    if(check.xy.dimensions==TRUE){
        temp <- cond[1]
        temp <- strsplit(temp, "[+]|[*]")[[1]]
        if(length(temp) > 2)
            stop("multiple 'x' and/or 'y' dimensions currently not allowed", call. = FALSE)
    }

#######################
#maybe work into extra.args
#in future version
#######################

    #export results
    d1
 
}






###########################
###########################
#
###########################
###########################




loaPlot <- function (x, data = NULL, panel = panel.xyplot, 
    ...) 
{

    ###################################
    #set up
    ###################################

    #ocall, ccall as of lattice  method
    #extra.args like previous

    ocall <- sys.call(sys.parent())
    ocall[[1]] <- quote(test)
    ccall <- match.call()
    extra.args <- list(...)

    #place data and panel

    ccall$data <- data
    ccall$panel <- function(..., subscripts){
                       panel = panelPal(..., subscripts=subscripts, panel=panel)
                   }

    #create loa plot configuration

    temp <- listUpdate(list(x=x, data=data), extra.args)
    d1 <- do.call(formulaHandler, temp)

    ..loa.x <- d1$x
    ..loa.y <- d1$y
    extra.args$z <- d1$z
    extra.args <- listUpdate(list(xlab = d1$x.name, ylab = d1$y.name, zlab = d1$z.name),
                             extra.args)
    x <- "..loa.y~..loa.x"
    if(!is.null(d1$panel.condition))
         x <- paste(x, d1$panel.condition, sep ="|")
    ccall$x <- as.formula(x)

    #add in z.condition
    if("z.condition" %in% names(d1))
        extra.args$z.condition <- d1$z.condition

    #defaults
#    extra.args <- listUpdate(list(pch=20, col = "blue"), extra.args)

    #
    ccall[names(extra.args)[names(extra.args)!=""]] <- extra.args[names(extra.args)[names(extra.args)!=""]]
    ccall[[1]] <- quote(lattice::xyplot)

    #make trellis open
    ans <- eval(ccall, parent.frame())

    #if panel has a preprocess component
    #update 
    loa.settings <- loaHandler(list(preprocess=TRUE, update=c("col", "cex")), 
                               extra.args)

    if(loa.settings$preprocess & "preprocess" %in% names(formals(panel))){

        preprocess <- function(x){
                          temp <- listUpdate(ans$panel.args[[x]], ans$panel.args.common)
                          temp <- listUpdate(temp, list(panel=panel, col.regions=1,
                                                        xlim = ans$x.limits, ylim = ans$y.limits, 
                                                        ignore=c("ylim", "xlim")))
                          do.call(panelPal, temp) 
        }

        prepro <- lapply(1:length(ans$panel.args), preprocess)
        for(i in names(prepro)){
            rng <- lapply(1:length(prepro), function(j)
                       range(prepro[[j]][i], na.rm=T))
            rng <- range(unlist(rng), na.rm=TRUE)
        }

        prepro <- lapply(1:length(prepro), function(j) {
                      temp <- prepro[[j]]
                      temp$subscripts <- 1:length(temp$x)
                      temp
                      })

####        ans$panel.args.common$at <- pretty(rng, 10)
####        ans$panel.args <- ick

        ans <- update(ans, preprocess = FALSE)
    }



return(ans)

#move these bits down


#     extra.args$cex <- do.call(cexHandler, extra.args)
#     extra.args$col <- do.call(colHandler, extra.args)$col





    ans$panel.args.common <- listUpdate(
                                 do.call(colHandler, listUpdate(extra.args, list(z=ans$panel.args.common$z))),
                                 ans$panel.args.common)

    ans <- update(ans, legend = ans$panel.args.common$legend)

    ans$call <- ocall
    ans
}







panel.test <- function (x, y, z = NULL, ..., n = 20, kernel.fun = NULL, panel.range = TRUE, process.step = 1:2) 
{


#loa.preprocess
#if null/no there run the lot
#if true just this first bit
#if false just the second

#if(is.null(loa.preprocess) || (loa.preprocess)){
#     [first bit]
#     [if not null and true return as list]
#     [unpack list] (do lot)
#}  
#[rest] [false or do all]


    extra.args <- list(...)

if(1 %in% process.step){


#print(x)
#print(y)

    temp <- length(x)
    mylist <- list(x = x, y = y, n = n)
    allowed <- unique(names(formals(MASS:::kde2d)))
    mylist <- listUpdate(mylist, extra.args, use = allowed)
 
#print(extra.args)

   if (panel.range & !"lims" %in% names(extra.args)) {
        lims <- if("xlim" %in% names(extra.args) & "ylim" %in% names(extra.args))
                    list(xlim = extra.args$xlim, ylim = extra.args$ylim) else 
                    current.panel.limits()

#print(lims)

#lims <- list(xlim=c(-4,4),ylim=c(-4,4))
#print(lims)

###############################
#fix this 
###############################



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

#print(kern.in)

if(!2 %in% process.step) return(kern.in)
} else {
    kern.in <- list(x=x, y=y, z=z)
}


#print(kern.in)

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





formulaHandler <- function(x, data = NULL, ..., check.xy.dimensions=TRUE){

    #extra.args
    extra.args <- list(...)

    if(!"loa.err.message" %in% names(extra.args))
        extra.args$loa.err.message <- "problem with x/data combination"

    #get base equation and z terms

    allowed.args <- names(formals(latticeParseFormula))
    temp <- listUpdate(list(dimension=3, multiple=TRUE, outer=TRUE), 
                             extra.args, use.b=allowed.args)
    temp <- temp[names(temp) != "model"]
    temp <- listUpdate(list(model=x, data=data), temp)

    d1 <- try(do.call(latticeParseFormula, temp), silent = TRUE)
    if(is(d1)[1] == "try-error")
        stop(extra.args$loa.err.message, call. = FALSE)

    #the end conditioning
    temp <- as.character(x)
    temp <- temp[length(temp)]
    cond <- strsplit(temp, "[|]")[[1]]
    if(length(cond)>1)
        d1$panel.condition <- cond[2]    


####################
#tidy these
####################

    #the z, z condition
    names(d1)[names(d1)=="left"] <- "z"
    names(d1)[names(d1)=="left.name"] <- "z.name"
    z <- d1$left
    if(length(d1$condition)>0){
        if(is.null(names(d1$condition)))
            names(d1)[names(d1)=="condition"] <- "z.condition" else
                if(length(d1$condition) > length(names(d1$condition)[names(d1$condition)!=""]))
                    d1$z.condition <- d1$condition[names(d1$condition)==""]
    }

    #x,y
    names(d1)[names(d1)=="right.x"] <- "x"
    names(d1)[names(d1)=="right.x.name"] <- "x.name"
    names(d1)[names(d1)=="right.y"] <- "y"
    names(d1)[names(d1)=="right.y.name"] <- "y.name"

#######################
#check for extra dimensions in x, y
#currently x or y not both!
#if allowed?
#######################
#may rethink this

    if(check.xy.dimensions==TRUE){
        temp <- cond[1]
        temp <- strsplit(temp, "[+]|[*]")[[1]]
        if(length(temp) > 2)
            stop("multiple 'x' and/or 'y' dimensions currently not allowed", call. = FALSE)
    }

#######################
#maybe work into extra.args
#in future version
#######################

    #export results
    d1
 
}





loaPlot <- function(x, data = NULL, #####groups = NULL,
    panel = panel.xyplot,
    ...){

    #package update - intended as common local framework
    #for plotting, e.g. for map plots...
    #karl 2012-08-16

    ##############
    #notes 
    ##############
    #use US spelling - more consistent with 'parent' lattice.
    #if using colorkey as arg need col in there
    #(or col arg gets assigned as colorkey if set!)
    #

    ##################
    #to do
    ##################
    #better alignment of default lims and cex
    #better legend/key control blocks or scale handling
    #tidier subscript handling
    #rethink aspect
    #

    ##################
    #to think about
    ##################
    #sort data before plot?
    #

    #####################
    #uses 
    #####################
    #lattice levelplot, latticeParseFormula
    #RColorBrewer (recoloring?)

    #not needed 'cos installation requirements

#    stopifnot(require("grid"))
#    stopifnot(require("lattice"))
#    stopifnot(require("RColorBrewer"))

    ####################
    #setups
    ####################

    extra.args <- list(...)
 
    ###########################
    #rework formula
    ###########################

    d1 <- do.call(formulaHandler, 
                  listUpdate(list(x=x, data=data), extra.args))

#################
#both z+z and |cond+cond are passed to conditioning
#need to separate
#################

#################
#currently x and y mutliple are mulitpled
#need to handle this better
#################


###################
#need to catch the z, x and y conditioning
###################

    #get xyplot x, y, etc,
    #note: these are taken from z ~ x * y 

    ..loa.x <- d1$x
    ..loa.y <- d1$y

####################
#no z conditioning tested for
####################

    if("z.condition" %in% names(d1))
        extra.args$z.condition <- d1$z.condition

##################
#lims need to be after cex
#  - need for cex/borders control
#but after col
#  - so zlim can be used with col.region?
#  - by colHandler
##################

    ######################
    #set plot defaults 
    #######################

    #general refs
    extra.args <- listUpdate(list(z = d1$z, ref = d1$x), 
                             extra.args)
 
    #cex
    extra.args$cex <- do.call(cexHandler, extra.args)

    #col, etc
    extra.args <- listUpdate(extra.args, 
                             do.call(colHandler, extra.args))
    
    #lims
    temp <- listUpdate(extra.args, list(x=d1$x, y=d1$y, z=d1$z))
    extra.args <- listUpdate(extra.args, 
                             do.call(limsHandler, temp))
    
##############
#this probably exists elsewhere?
#need better tie in with cex?
##############

################
#need to tie zlim into cols
################

    #set defaults
    #then update - so user default supercede 
    temp <- list(aspect = NULL, pch=20)
    
    extra.args <- listUpdate(temp, extra.args)

    #rearrange formula for xyplot
    #keeping any conditioning
    x <- "..loa.y~..loa.x"
    if(!is.null(d1$panel.condition))
         x <- paste(x, d1$panel.condition, sep ="|")
    x <- as.formula(x)
        
    #forced to expanded
    extra.args <- listUpdate(extra.args, 
                      listExpand(extra.args, ignore=c("xlim", "ylim", "zlim", "at", "col.regions", 
                                                      "aspect", "ref", "layout"), 
                          ref = extra.args$ref)
                  )

    #########################
    #plot data
    #########################

    #use update method to allow
    #user fine control
    temp <- list(x = x, data = data, 
                 xlab = d1$x.name,
                 ylab = d1$y.name,
                 panel = function(..., subscripts){
                                  panel = panelPal(..., subscripts=subscripts, panel=panel)
                         }

       )

    extra.args <- listUpdate(temp, extra.args)
    ans <- do.call(xyplot, extra.args) 
   
    #messy but means <-... still updates
    #might rethink
    plot(ans)
    invisible(ans)       

}




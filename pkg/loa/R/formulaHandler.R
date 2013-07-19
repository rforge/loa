#in development code
#[1 -TBC] functions 



#######################
##might want to make 
##own space for conds
#######################


#formulaHandler - handles the x formula
#stripHandler   - handles strips

#urgent
##########################
#stripHandler 
##########################
#temp fix for cond 
#need a better fix
#

#fixes 
#to dos
#suggestions







###########################
###########################
#formulaHandler
###########################
###########################

formulaHandler <- function(x, data = NULL, ..., formula.type="z~x*y|cond", 
                           panel.zcases = FALSE,
                           check.xy.dimensions=TRUE, output = "extra.args"){

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

##we get warning here if a duplicated term 
##is used in formula z component
##e.g. z1 + z1 ~ a * b in 
##suppressWarnings would hide this


##next bit is needed for end tests
##if not using anywhere else anymore
##could do better/combine at top

    #the end conditioning
    temp <- as.character(x)
    temp <- temp[length(temp)]
    cond <- strsplit(temp, "[|]")[[1]]
#    if(length(cond)>1)
#        d1$panel.condition <- cond[2]    



####################
#tidy these
####################

    #the z, z condition
    names(d1)[names(d1)=="left"] <- "z"
    names(d1)[names(d1)=="left.name"] <- "z.name"
    z <- d1$left


##this fails if cond a>0 + b>0
##but not g + g even when g = a>0
##but so lattice xyplot!
##see what panel.condition is then

##also z1+z2 does not set zcases


    if(!is.null(d1$condition)){
        if(is.null(names(d1$condition)))
            d1$zcases <- d1$condition[[1]] else 
                if("" %in% names(d1$condition))
                    d1$zcases <- d1$condition[[which(names(d1$condition) == "")]]

        if(is.factor(d1$zcases)){
            levels(d1$zcases) <- make.unique(levels(d1$zcases))
            d1$z.name <- paste(levels(d1$zcases), collapse = " + ")
        }
        
        if(any(names(d1$condition) != ""))
            d1$panel.condition <- d1$condition[names(d1$condition) != ""]

    }
    d1$condition <- NULL


##    if(length(d1$condition)>0){
##        if(is.null(names(d1$condition)))
##            names(d1)[names(d1)=="condition"] <- "zcases" else
##                if(length(d1$condition) > length(names(d1$condition)[names(d1$condition)!=""]))
##                    d1$zcases <- d1$condition[names(d1$condition)==""]
##        for(i in 1:length(d1$zcases)){
#testing make these unique
##            levels(d1$zcases[[i]]) <- make.unique(levels(d1$zcases[[i]]))
##        }
#testing remove list, so it vectorises
##        d1$zcases <- d1$zcases[[1]]

##    }

    #x,y
    if(formula.type=="z~y*x|cond"){
        names(d1)[names(d1)=="right.y"] <- "x"
        names(d1)[names(d1)=="right.y.name"] <- "x.name"
        names(d1)[names(d1)=="right.x"] <- "y"
        names(d1)[names(d1)=="right.x.name"] <- "y.name"
    } else {
        names(d1)[names(d1)=="right.x"] <- "x"
        names(d1)[names(d1)=="right.x.name"] <- "x.name"
        names(d1)[names(d1)=="right.y"] <- "y"
        names(d1)[names(d1)=="right.y.name"] <- "y.name"
    }


#######################
#check for extra dimensions in x, y
#currently x or y not both!
#if allowed?
#######################
#may rethink this
#move to top?


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

    #structure zcases
    if("zcases" %in% names(d1) && panel.zcases){
        d1$panel.condition <- listUpdate(list(zcases = d1$zcases), d1$panel.condition)
        d1$zcases <- NULL
    }

    #export results if lattice.like
    if(output=="lattice.like") return(d1)

    #temp fix for conditioning labels
    extra.args <- do.call(stripHandler,
                          listUpdate(list(striplab = names(d1$panel.condition)), extra.args)
                         )


    ..loa.x <- d1$x
    ..loa.y <- d1$y

     extra.args$z <- d1$z
     extra.args$ref <- d1$x
     extra.args$groups <- d1$groups
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

    
    extra.args$x <- as.formula(x)

    extra.args
 
}





####################################
####################################
##stripHandler
####################################
####################################


stripHandler <- function(..., striplab=NULL){

##########################
#messy 
#needs a rethink
##########################

##########################
#pass list 
#with listLoad handling
##########################

    extra.args <- list(...)

    #if not striplab
    #nothing to do
    if(is.null(striplab)) return(extra.args)

    if(!"strip" %in% names(extra.args)) extra.args$strip <- TRUE 

    if("strip" %in% names(extra.args)){

        if(is.logical(extra.args$strip) && !extra.args$strip) return(extra.args)

        temp <- if(is.function(extra.args$strip)) extra.args$strip else strip.default

        extra.args$strip <- function(var.name, ...) temp(..., var.name=striplab)

    }

    return(extra.args)
}



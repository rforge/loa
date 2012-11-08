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








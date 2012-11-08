#in development code
#[TBC - NUMBER] functions 

#panelPal
#panelPal2
#getArgs
#getPlotArgs
#isGood4LOA
#parHandler

#loaHandler (not exported)


#NOTE: much borrowed from lattice 

#to do
#callWithThis document or drop
#very minor

#panelPal needs a do not subscript...


##############################
##############################
##panelPal
##############################
##############################


#this needs a lot of tidying

panelPal <- function(x, y, subscripts, at, col.regions, ..., 
                    panel = panel.xyplot, ignore = NULL,
                    group.fun = NULL){

           #############
           #got to be better way of doing this
           #make all panel-specific


###########
#print("HI")
##########
           extra.args <- list(...)
           temp <- if(is.null(ignore))
                       extra.args else extra.args[!names(extra.args) %in% ignore]

           if(!is.null(subscripts)){
               temp <- lapply(temp, function(x)
                           x <- if(length(x)>1) x[subscripts] else x )
                               subscripts <- 1:length(subscripts)
           }
           temp <- listUpdate(list(x = x, y = y, z = temp$z, at = at, 
                              col.regions=col.regions, subscripts=subscripts),
                              temp)
           
           if(!is.null(ignore))
               temp <- listUpdate(temp, extra.args[names(extra.args) %in% ignore])
   
###########
#print(names(temp))
###########

           if(!"groups" %in% names(temp)) return(do.call(panel, temp))

#note this action 

          groups <- temp$groups
          temp <- temp[names(temp) != "groups"]
 


           grp <- if(is.factor(groups)) levels(groups) else unique(groups)
  ##         temp.fun <- function(temp, i){
  ##                         temp2 <- lapply(temp, function(x) x <- if (length(x) > 1) 
   ##                                    x[groups==i] else x)
   ##                        
   ##                    }



#does the next bit need nicer coloring?
#does the next bit an option from above
           
          if(is.null(group.fun)){
              group.fun <- list()
              for(i in grp){
                  group.fun[[i]] <- list(col= i, pch = i)
              }}

#print(group.fun[1])


           for(i in grp){

##add catcher for missing 
##group.fun
##update col and symbol 
## if mono?

                temp2 <- lapply(temp, function(x) x <- if (length(x) > 1) 
                                x[groups==i] else x)


###remove later
#           print(temp2$x)
####


#does the next bit need error catching
#only do else if function?

#does the next bit need 

           if(is.list(group.fun[[i]]))
                do.call(panel, listUpdate(temp2, group.fun[[i]])) else 
                do.call(group.fun[[i]], temp2)
           }


}


##############################
##############################
##panelPal2
##############################
##############################


panelPal2 <- function(ans, panel = NULL, preprocess = FALSE, safe.mode = TRUE,
         reset.xylims = FALSE){

#panelPal v2
#kr

#to do
#######################
#update xy limits after process
#######################
#group handling
#######################
#

#to think about
#######################
#drop ref from common after transfer?
#######################
#col/cex handling when either are 
#safe.mode managed  
#######################
#

    #move all required and expanded information to panel.args[[1]]
    #drop ignores
    #make expanded list

    if(!"xlim" %in% names(ans$panel.args.common))
        ans$panel.args.common$xlim <- ans$x.limits
    if(!"ylim" %in% names(ans$panel.args.common))
        ans$panel.args.common$ylim <- ans$y.limits

    ignore <- c("xlim", "ylim", "zlim", "xlab", "ylab", "zlab", "main",
                "at", "ref", "col.regions")
    if(safe.mode){
        if("loa.settings" %in% names(formals(panel))){
            temp <- panel(loa.settings=TRUE)
            ignore <- unique(c(ignore, temp$plot.args, temp$panel.args))
        }
    }
    
    transfers <- sapply(names(ans$panel.args.common), 
                            function(x) length(ans$panel.args.common[[x]])>1)
    transfers <- names(ans$panel.args.common)[transfers]
    transfers <- ans$panel.args.common[transfers[!transfers %in% ignore]]
    transfers <- listExpand(transfers, ref = ans$panel.args.common$ref)

    #if something to work with
    #transfer subscripted versions to panels
    #wipe these from common
    if(length(transfers)>0){
        temp <- lapply(ans$panel.args, function(y)
                       listUpdate(y, lapply(transfers, function(x) x[y$subscripts])))
        ans$panel.args <- temp
        ans$panel.args.common <- ans$panel.args.common[!names(ans$panel.args.common) %in% names(ans$panel.args[[1]])]
    }

#does adding panel here 
#speed anything up?

    #proper panel
    if(!is.null(panel))
        ans$panel <- panel

    #if processing process
    if(preprocess){
        if("process" %in% names(formals(ans$panel)) && formals(ans$panel)$process){
            out <- lapply(1:length(ans$panel.args), function(x){
                              temp <- listUpdate(ans$panel.args.common, ans$panel.args[[x]])
                              temp <- listUpdate(temp, list(process=TRUE, plot=FALSE))
#####################
#here groups is in temp if it is sent
#so it could be handled as previous?
#see
#print(names(temp))

                              temp <- do.call(ans$panel, temp)
##################
#group handling goes in here?
##################

                              listUpdate(ans$panel.args[[x]], temp) 
                          })
            ans$panel.args <- out
            formals(ans$panel)$process <- FALSE
            formals(ans$panel)$plot <- TRUE    

        #update panel.args
        #run panel
        }
    }

#TO DO
#########################
#update xylims if requested
#########################

    ##########################
    #update lims for modified 
    #panel.args[[n]] elements
    ##########################

    #get names of elements in panel.args[[1]] that are numeric
    ranges <- sapply(names(ans$panel.args[[1]]), 
                        function(x) is.numeric(ans$panel.args[[1]][[x]]))
    ranges <- names(ans$panel.args[[1]])[ranges]

    #remove common names 
    ranges <- ranges[!ranges %in% c("x", "y", "subscripts")]

    #remove elements for which lims are set in panel.args.common
    temp <- gsub("lim$", "", grep("lim$", names(ans$panel.args.common), value=T))
    ranges <- ranges[!ranges %in% temp]

    #recalculate lims for these
    temp.fun <- function(x){
                    range(lapply(ans$panel.args, function(y){
                              range(y[[x]], na.rm=TRUE, finite=TRUE)
                          }), na.rm=TRUE, finite=TRUE)
                }
    if(length(ranges)>0){
        temp <- lapply(ranges, temp.fun)
        names(temp) <- paste(ranges, "lim", sep="")
        ans$panel.args.common <- listUpdate(ans$panel.args.common, temp)
    }

    ###################
    #update cex and col
    ###################
#tidy this

    temp <- listUpdate(ans$panel.args.common, list(z=ans$panel.args.common$zlim, output="all"))
    temp <- do.call(colHandler, temp)
    temp <- temp[!names(temp) %in% c("col", "z")]
#    ans$panel.args.common <- listUpdate(ans$panel.args.common, temp)
    ans <- do.call(function(...) update(ans, ...), temp)

    temp <- lapply(ans$panel.args, function(x){
                       temp <- listUpdate(ans$panel.args.common, x)
                       temp$col <- do.call(colHandler, temp)
                       temp$cex <- do.call(cexHandler, temp)
                       temp[!names(temp) %in% names(ans$panel.args.common)]
                   })
    ans$panel.args <- temp

    ###############################
    #return modified trellis object
    ###############################

    return(ans)

}







##############################
##############################
##getPlotArgs
##############################
##############################



getArgs <- function(source = TRUE, 
                    local.resets = TRUE, user.resets = TRUE, 
                    is.scales.lines = FALSE, elements = NULL, ..., 
                    defaults = list(), defaults.only = FALSE){

    #takes something and returns args for plot component
    #uses trellis.par.get
    #axis.line, axis.text, etc,    
    #names(trellis.par.get()) for list

    #could be rationalised further

    #targets  
    targets <- if(defaults.only)
                   c(names(defaults), "isGood4LOA") else NULL

    #fix.source function
    fix.source <- function(n, a = 1, b = 1, sc.el, is.sc){
        if(is.null(n)) return(list(isGood4LOA = FALSE))
        if(is.logical(n))
            if(n) return(list()) else return(list(isGood4LOA = FALSE))
        if(is.list(n)){
            if(!is.null(sc.el) && sc.el %in% names(n)){
                if(is.vector(n[[sc.el]]) & !is.list(n[[sc.el]])){
                    if(length(n[[sc.el]])<b) n[[sc.el]] <- zHandler(n[[sc.el]], TRUE, 1:b)
                    n[[sc.el]] <- list(col = n[[sc.el]][seq(a,length(n[[sc.el]]), by = b)])    
                }
                n <- listUpdate(n, n[[sc.el]])
                n <- n[names(n) != sc.el]
                if(is.sc){
                    n <- n[!names(n) %in% c("col", "alpha")]
                    #then replace with .line if present
                    if("col.line" %in% names(n))
                        names(n)[names(n)=="col.line"] <- "col"
                    if("alpha.line" %in% names(n))
                        names(n)[names(n)=="alpha.line"] <- "alpha"
                }
            }
            return(n)
        }
        if(is.data.frame(n)) return(as.data.frame(n))
        #assuming vector
        if(length(n)<b) n <- zHandler(n, TRUE, 1:b)
        list(col = n[seq(a,length(n), by = b)])     

     }

    #element handler
    el.handler <- function(def, sc.el, a=1, b=1, so, lr, ur, is.sc=FALSE){

                      output <- listUpdate(def, fix.source(so, sc.el = sc.el, a = a, b = b, is.sc = is.sc),
                                           use.b = targets)
                      output <- listUpdate(output, fix.source(lr, sc.el = sc.el, a = a, b = b, is.sc = FALSE),
                                           use.b = targets)
                      listUpdate(output, fix.source(ur, sc.el = sc.el, a = a, b = b, is.sc = FALSE),
                                 use.b = targets)    
    }


    #handle element(s)
    #messy but don't want list if only one element
    if(is.null(elements)) return(el.handler(defaults, NULL, 1, 1, source, local.resets, user.resets, is.scales.lines))
    if(length(elements)<2) return(el.handler(defaults, elements, 1, 1, source, local.resets, user.resets, is.scales.lines)) 

    output <- lapply(1:length(elements), function(x)
                         el.handler(defaults, elements[x], x, length(elements), source, local.resets, user.resets, is.scales.lines))

    names(output) <- elements
    output

}





##############################
##############################
##getArgs
##getPlotArgs
##############################
##############################



getPlotArgs <- function(defaults.as = "axis.line", source = TRUE, local.resets = TRUE, user.resets = TRUE, 
                        elements = NULL, ..., is.scales.lines = NULL, defaults.only = TRUE){

    #check defaults.as
    if(!defaults.as[1] %in% names(trellis.par.get())){
         warning("could not recover 'default.as' [guessing axis.text]", call.=FALSE)
         defaults.as <- "axis.text"
    }

    if(is.null(is.scales.lines))
        is.scales.lines <- if(is.character(defaults.as)  && length(grep("line", defaults.as)) > 0)
                               TRUE else FALSE

    #get defaults
    defaults <- trellis.par.get(defaults.as[1])

    getArgs(source = source, local.resets = local.resets, user.resets = user.resets, 
            is.scales.lines = is.scales.lines, defaults.only = defaults.only, defaults = defaults, 
            elements = elements)

}





##############################
##############################
##isGood2Plot
##############################
##############################

#temp function

#arg could be replaced with ...
#function could be recursive
#and would be needed

isGood4LOA <- function(arg){

    if(is.null(arg)) return(FALSE)
    if(is.logical(arg) && !all(arg)) return(FALSE)
    if(is.list(arg) && !is.null(arg$isGood4LOA) && !arg$isGood4LOA) return(FALSE)
    TRUE

}





########################
########################
##parHandler
########################
########################

parHandler <- function (scheme = NULL, ...) {


#######################
#urgent
#######################
#rethink the inputs
#

#think about order
#theme first means unnamed get assigned as this

#could hide this in ...
#then
#if theme named
#if output set par.settings
#

#could use theme and set.theme
#then if garbage sent it will ignore

    extra.args <- list(...)

    ###################
    #pre-defined themes
    ###################

    myschm <- NULL

    if(is.character(scheme)){

        if (scheme == "greyscale") {
            symbol <- gray(1:8/8)
            fill <- "grey"
            region <- gray(11:1/11)
            reference <- "black"
            bg <- "transparent"
            fg <- "black"
            myschm <- list(plot.polygon = list(col = fill[1], border = fg[1]), 
                         box.rectangle = list(col = symbol[1]), box.umbrella = list(col = symbol[1]), 
                         dot.line = list(col = reference), dot.symbol = list(col = symbol[1]), 
                         plot.line = list(col = symbol[1]), plot.symbol = list(col = symbol[1]), 
                         regions = list(col = colorRampPalette(region)(100)), 
                         reference.line = list(col = reference), superpose.line = list(col = symbol), 
                         superpose.symbol = list(col = symbol), superpose.polygon = list(col = fill, 
                         border = fg), background = list(col = bg), add.line = list(col = fg), 
                         add.text = list(col = fg), box.dot = list(col = fg), 
                         axis.line = list(col = fg), axis.text = list(col = fg), 
                         strip.border = list(col = fg), strip.background = list(col = "white"), 
                         strip.shingle = list(col = "grey"), box.3d = list(col = fg), 
                         par.xlab.text = list(col = fg), par.ylab.text = list(col = fg), 
                         par.zlab.text = list(col = fg), par.main.text = list(col = fg), 
                         par.sub.text = list(col = fg))
        }

        if (scheme == "kr.web") {
            symbol <- colHandler(1:8, col.regions="Blues")$col
            fill <- "white"
            region <- colHandler(1:11, col.regions="Blues")$col
            reference <- "white"
            bg <- "black"
            fg <- "white"
            myschm <- list(plot.polygon = list(col = fill[1], border = fg[1]), 
                         box.rectangle = list(col = symbol[1]), box.umbrella = list(col = symbol[1]), 
                         dot.line = list(col = reference), dot.symbol = list(col = symbol[1]), 
                         plot.line = list(col = symbol[1]), plot.symbol = list(col = symbol[1]), 
                         regions = list(col = colorRampPalette(region)(100)), 
                         reference.line = list(col = reference), superpose.line = list(col = symbol), 
                         superpose.symbol = list(col = symbol), superpose.polygon = list(col = fill, 
                         border = fg), background = list(col = bg), add.line = list(col = fg), 
                         add.text = list(col = fg), box.dot = list(col = fg), 
                         axis.line = list(col = fg), axis.text = list(col = fg), 
                         strip.border = list(col = fg), strip.background = list(col = "black"), 
                         strip.shingle = list(col = "lightgrey"), box.3d = list(col = fg), 
                         par.xlab.text = list(col = fg), par.ylab.text = list(col = fg), 
                         par.zlab.text = list(col = fg), par.main.text = list(col = fg), 
                         par.sub.text = list(col = fg))
        }

    }

    if(is.list(scheme))
        myschm <- scheme
    
    if(is.list(myschm)){
        if (is.null(extra.args$par.settings)) 
            extra.args$par.settings <- myschm
        else extra.args$par.settings[!names(myschm) %in% names(extra.args$par.settings)] <- myschm[!names(myschm) %in% 
            names(extra.args$par.settings)]
    }

#warning if myschm not list?

#return par.setting or extra.args?

    extra.args$par.settings
}









##############################
##############################
##loaHandler
##############################
##############################



loaHandler <- function(loa.settings = NULL, ...){

    #set up
    extra.args <- list(...)

    #generate list of loa.settings
    if(is.null(loa.settings) || !is.list(loa.settings))
        loa.settings <- list()

    temp <- grep("^loa[.]", names(extra.args))
    if(length(temp)>0){
        extra.args <- extra.args[temp]
        names(extra.args) <- gsub("^loa[.]", "", names(extra.args))
        loa.settings <- listUpdate(loa.settings, extra.args)  
    }

    #output
    loa.settings

}







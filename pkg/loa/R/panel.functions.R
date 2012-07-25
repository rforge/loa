#in development code
#[TBC - NUMBER] functions 

#panelPal
#getArgs
#getPlotArgs
#isGood4LOA


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

#in development code
#[TBC - NUMBER] functions 

#listHandler
#listUpdate
#listExpand


#NOTE: much borrowed from lattice 

##################
#possibles
##################
#possibly rethink drop.dots
#so it can drop stuff
#####
#should listExpand be listExpandVectors?
#because that is more descriptive
#

##################
#to do
##################
#rethink ignore handling/descriptions
#could also have 




############################
############################
##listHandler
############################
############################

listHandler <- function(a, use = NULL, ignore = NULL, 
                        drop.dots=TRUE){
    
    if(drop.dots)
        a <- a[names(a) != "..."]
    if(!is.null(use))
        a <- a[names(a) %in% use]
    if(!is.null(ignore))
        a <- a[!names(a) %in% ignore]

    a

}



#####################
#####################
##localUpdate
#####################
#####################

#local function to update lists
#[in development]

listUpdate <- function(a, b, use = NULL, ignore = NULL,
                       use.a = use, use.b = use,
                       ignore.a = ignore, ignore.b = ignore, 
                       drop.dots = TRUE){

    a <- listHandler(a, use.a, ignore.a, drop.dots)
    b <- listHandler(b, use.b, ignore.b, drop.dots)

    if(length(names(b) > 0))
        a <- modifyList(a, b)
    a
}




############################
############################
##listExpand
############################
############################

listExpand <- function(a, ref = NULL, use = NULL, 
                       ignore = NULL, drop.dots = TRUE){

    a <- listHandler(a, use, ignore, drop.dots)

    if(is.null(ref))
        return(a)

    temp <- lapply(a, function(x){x <- if(is.vector(x) & !is.list(x)){
                             if(length(x) > 1 & length(x) < length(ref))
                                 rep(x, ceiling(length(ref)/length(x)))[1:length(ref)] else x
                             } else x 
                         })
    listUpdate(a, temp)
}





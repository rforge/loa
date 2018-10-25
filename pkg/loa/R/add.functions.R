#indevelopment

###################
#add functions
###################

######################
#function (exported)
######################

# add_loaPanel (export)

#########################
#to do
#########################

#lot of stuff



########################
#functions
########################

########################
#add_loaPanel
########################

add_loaPanel <- function(lattice.plot=trellis.last.object(),
                         preprocess = NULL,
                         panel =NULL, postprocess = NULL,
                      ...){
  x.args <- list(...)
  if(!is.null(preprocess)) 
    lattice.plot <- do.call(preprocess, listUpdate(x.args,
                                 list(lattice.plot=lattice.plot)))
  if(!is.null(panel)){
    pre.panel <- lattice.plot$panel
    lattice.plot$panel <- function(...){
      pre.panel(...)
      #pass args in add... function to this panel?
      do.call(panel, listUpdate(list(...), x.args))
    }
  }
  if(!is.null(postprocess)) 
    lattice.plot <- do.call(postprocess, listUpdate(x.args,
                                 list(lattice.plot=lattice.plot)))
  lattice.plot  
}


##########################
#working code
##########################

#quick function to add points to an existing loaPlot

#look at why it does not work if x, y and z do not match
#might not need native
#might only work with trellis.last object?
#might need to work formula and data into this....
#might also be able to make one main function 
#that you pass different panels to for many of these...
#might need to reorder the listUpdate terms....
#might want to control is new bit goes top or bottom...



addXY <- function (x=NULL, y=NULL, z=NULL, ..., unit = "native", scale.correction = NULL, object = trellis.last.object()){ 

   #add points to an existing trellis.plot
   extra.args <- list(...)
   panel <- object$panel
   object$panel <- function(...){
                         panel(...)
                         do.call(panel.loaPlot, listUpdate(extra.args, list(x=x, y=y, z=z)))
                    }
   object
}


#same for text

addText <- function (x=NULL, y=NULL, text=NULL, ..., unit = "native", scale.correction = NULL, object = trellis.last.object()){ 

   #add text to an existing trellis.plot
   extra.args <- list(...)
   panel <- object$panel
   object$panel <- function(...){
                         panel(...)
                         do.call(panel.text, listUpdate(extra.args, list(x=x, y=y, labels=text)))
                    }
   object
}


#add ghost points in multipanel loaPlots

addGhosts <- function (..., unit = "native", scale.correction = NULL, object = trellis.last.object()){ 

   #add ghost points to an existing plot
   extra.args <- list(...)
   temp <- object$panel.args
   if(length(temp)==1) warning("ghosts won't help...")
   x <- lapply(temp, function(x) x$x)
   x <- do.call(c, x)
   y <- lapply(temp, function(x) x$y)
   y <- do.call(c, y)
   z <- lapply(temp, function(x) x$z)
   z <- do.call(c, z)
   panel <- object$panel
   object$panel <- function(...){
                        do.call(panel.loaPlot, listUpdate(extra.args, list(x=x, y=y, z=z, col="grey", alpha=0.25)))
                        panel(...)
                    }
   object
}


#add trend line(s) to an existing plot

addTrendLine <- function(..., object=trellis.last.object()){

#work this up
#1. need to repeat for each panel 
#   currently only looks at panel.args[[1]]
#2. need to be able to set fit function, formula and defaults in call
#   currently hardcoded as loess, etc.
#3. might need to set line resolution
#   currently 100 across data x range. could look as xlim?
#4. need to be able to control line and polygon properties from call
#   currently all hardcoded in panel function update
#5. might want to for loop the polygon drawing steps

#issues
#1. dies if x is date/time class
 

    #quick function to add fit line to lattice plot
    
    x <- object$panel.args[[1]]$x
    y <- object$panel.args[[1]]$y

    mod <- loess(y~x)
    new.x <- range(x, na.rm=TRUE)
    new.x <- seq(new.x[1], new.x[2], length.out=100)
    out <- predict(mod, se=TRUE, newdata=data.frame(x=new.x))

    #plot updates
    panel <- object$panel
    object$panel <- function(...){
                    panel(...)
                    panel.polygon(x=c(new.x, rev(new.x)), 
                                  y=c(out$fit+(3*out$se.fit), rev(out$fit-(3*out$se.fit))), 
                                  border=FALSE, col=2, alpha=0.1)
                    panel.polygon(x=c(new.x, rev(new.x)), 
                                  y=c(out$fit+(2*out$se.fit), rev(out$fit-(2*out$se.fit))), 
                                  border=FALSE, col=2, alpha=0.1)
                    panel.polygon(x=c(new.x, rev(new.x)), 
                                  y=c(out$fit+(1*out$se.fit), rev(out$fit-(1*out$se.fit))), 
                                  border=FALSE, col=2, alpha=0.1)
                    panel.xyplot(x=new.x, y=out$fit, col=2, lwd=2, type="l")
                }

    #outputs
    plot(object)
    invisible(mod)
}




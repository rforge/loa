#indevelopment

###################
#addXYFit functions
###################

######################
#function (exported)
######################

# addXYLMFit 
# addXYLOESSFit

# addXYFit_prep
# loaXYFit_lm
# loaXYFit_loess 
# panel.loaXYFit 

#########################
#to do
#########################

#lot of stuff



########################
#functions
########################


###################
#addXYLMFit
###################

addXYLMFit <- function(lattice.plot=trellis.last.object(),
                      preprocess = addXYFit_prep,
                      panel=panel.loaXYFit, ...){
  add_loaPanel(lattice.plot=lattice.plot, ...,
               preprocess=preprocess, panel=panel)
}

####################
# addXYLOESSFit
####################

addXYLOESSFit <- function(lattice.plot=trellis.last.object(),
                      preprocess = addXYFit_prep,
                      model.method=loaXYFit_loess,
                      panel=panel.loaXYFit, ...){
  add_loaPanel(lattice.plot=lattice.plot, 
               model.method=loaXYFit_loess, 
               ..., preprocess=preprocess, panel=panel)
}


#########################
#addXYFit_prep
#########################

addXYFit_prep <- function(lattice.plot=trellis.last.object(),
                              model.method=loaXYFit_lm,
                             ...){
  
  common <- lattice.plot$panel.args.common
  #tidy group/zcase args
  if(!"group.ids" %in% names(common)){
    #might need warning re zcases...
    common$group.ids <- if("groups" %in% common){
      if(is.factor(common$groups)) 
        levels(common$groups) else unique(common$groups)
    } else {
      "default"
    }
    #might need group.args?
  }
  ans <- lapply(lattice.plot$panel.args, 
      function(x){
          temp <- listUpdate(common, x)
          if(!"groups" %in% names(temp)){
            temp$groups <- rep(temp$group.ids[[1]], 
                               length(temp$x))
          }
          out <- lapply(temp$group.ids, function(i){
            x <- temp$x[temp$groups==i]
            y <- temp$y[temp$groups==i]
            model.method(x=x, y=y, ..., 
                         group.id=as.character(i))
          })
          names(out) <- temp$group.ids
          out
          #if no groups, make default
          #for each group
          #make x,y, 
          #output fit.mod, mod.x, mod.y, mod.se, rep,
      })
  #this right place?
  lattice.plot$panel.args.common$loa.mod.fit <- ans
  #output
  lattice.plot
}


###########################
#loaXYFit_lm
###########################

loaXYFit_lm <- function(x, y, output.len=25, 
                        formula.signif=2, r2.digits=3, 
                        group.id=NULL, ...){

  if(length(x)<3 || length(y)<3)
    return(list(mod=NULL, mod.x=NULL, mod.y=NULL, 
                mod.se=NULL, formula=NULL, r2=NULL))
  mod <- lm(y~x)
  mod.x <- seq(min(x, na.rm=TRUE), 
                 max(x, na.rm=TRUE), length.out=output.len)
  temp <- predict(mod, newdata=data.frame(x=mod.x),
                       se.fit=TRUE)
  mod.y <- rep(NA, length(mod.x))
  mod.y[as.numeric(names(temp$fit))] <- temp$fit
  mod.se <- rep(NA, length(mod.x))
  mod.se[as.numeric(names(temp$se.fit))] <- temp$se.fit
  int <- coef(mod)["(Intercept)"]
  m <- coef(mod)["x"]
  formula <- paste("y = ", signif(m,formula.signif), "x", sep="")
  if(int<0)
    formula <- paste(formula, " - ", - signif(int, 
                                              formula.signif),
                     sep="")
  if(int>0)
    formula <- paste(formula, " + ", signif(int,
                                            formula.signif),
                     sep="")
  formula
  r2 <- summary(mod)$r.squared
  r2 <- paste("(", round(r2,digits=r2.digits), ")", sep="")
  r2

  
  #needs
  #formula and r2

  list(mod=mod, mod.x=mod.x, mod.y=mod.y, mod.se=mod.se,
       formula=formula, r2=r2)
}


###################
# loaXYFit_loess
###################

loaXYFit_loess <- function(x, y, output.len=25, 
                        r2.digits=3, 
                        group.id=NULL, ...){

  if(length(x)<3 || length(y)<3)
    return(list(mod=NULL, mod.x=NULL, mod.y=NULL, 
                mod.se=NULL, formula=NULL, r2=NULL))
  mod <- loess(y~x)
  mod.x <- seq(min(x, na.rm=TRUE), 
                 max(x, na.rm=TRUE), length.out=output.len)
  temp <- predict(mod, newdata=data.frame(x=mod.x),
                       se=TRUE)
  
  mod.y <- rep(NA, length(mod.x))
  mod.y[as.numeric(names(temp$fit))] <- temp$fit
  mod.se <- rep(NA, length(mod.x))
  mod.se[as.numeric(names(temp$se.fit))] <- temp$se.fit
  if(group.id=="default") 
    group.id <- "all.data"
  formula <- paste("loess(", group.id, ")", sep="")
  
  #calc r2 because loess does not...
  temp <- data.frame(y=y[!is.na(y) & !is.na(x)], x=predict(mod))
  r2 <- summary(lm(y~x, temp))$r.squared
  
  #r2 <- summary(mod)$r.squared
  r2 <- paste("(", round(r2,digits=r2.digits), ")", sep="")
  
  list(mod=mod, mod.x=mod.x, mod.y=mod.y, mod.se=mod.se,
       formula=formula, r2=r2)
}



#############################
# panel.loaXYFit
#############################

panel.loaXYFit <- function(...){
  #setup
  plot.args <- listUpdate(list(fit=TRUE, se=TRUE, report=TRUE), 
                          list(...))
  if(!"group.ids" %in% names(plot.args)){
    plot.args$group.ids <- "default"
    if(!"groups" %in% names(plot.args))
      plot.args$groups <- rep("default", length(plot.args$x))
  }
#sort out col, lty and lwd
  if(!"col" %in% names(plot.args)){
    plot.args$col <- do.call(colHandler, 
                      listUpdate(plot.args,
                         list(ref=1:length(plot.args$group.ids))))
  }
  if(!"lty" %in% names(plot.args))
    plot.args$lty <- rep(1, length(plot.args$group.ids))
  if(!"lwd" %in% names(plot.args))
    plot.args$lwd <- rep(1, length(plot.args$group.ids))

  all.mods <- if("loa.mod.fit" %in% names(plot.args))
#this needs tidying
    plot.args$loa.mod.fit[[panel.number()]] else {
      temp2 <- addXYFit_prep(list(panel.args.common=plot.args,
                             panel.args=list(list())))
      temp2$panel.args.common$loa.mod.fit[[1]]
  }
  
  #se and fit
  for(i in names(all.mods)){
    mod <- all.mods[[i]]
    #this might need more b+b...
    if(!is.null(mod$mod.x)){
      #se settings
      if(isGood4LOA(plot.args$se)){
        m.args <- listUpdate(plot.args, 
                           do.call(listLoad, 
                                   listUpdate(plot.args, 
                      list(load="se")))[["se"]])
        m.args <- listUpdate(list(group.args="col",
                                levels=3,
                                alpha=0.75, border=FALSE), 
                           m.args)
        for(j in m.args$group.args){
          if(j %in% names(m.args))
            m.args[[j]] <- m.args[[j]][m.args$group.ids==i]
        }
        m.args$alpha <- m.args$alpha/m.args$levels
        for(k in c(m.args$levels:1)){
          m.args$x <- c(mod$mod.x, rev(mod$mod.x))
          m.args$y <- c(mod$mod.y+(k*mod$mod.se), 
                    rev(mod$mod.y-(k*mod$mod.se)))
          do.call(panel.polygon, m.args)
        }
      }
      if(isGood4LOA(plot.args$fit)){
        m.args <- listUpdate(plot.args, 
                           do.call(listLoad, 
                                   listUpdate(plot.args, 
                      list(load="fit")))[["fit"]])
        m.args <- listUpdate(list(group.args=c("col", "lty"), 
                                  type="l"), 
                           m.args)
        m.args$group.args <- c("col", "lty", "lwd")
        for(j in m.args$group.args){
          if(j %in% names(m.args))
            m.args[[j]] <- m.args[[j]][m.args$group.ids==i]
        }
        m.args$x <- mod$mod.x
        m.args$y <- mod$mod.y
        m.args$subscripts <- 1:length(m.args$x)
        do.call(panel.xyplot, m.args)
      }
    }
  }
  #report
  if(isGood4LOA(plot.args$report)){
    m.args <- listUpdate(plot.args, 
                        do.call(listLoad, 
                                listUpdate(plot.args, 
                        list(load="report")))[["report"]])
#need tidying
    m.args <- listUpdate(list(position=c(0.15,0.85)), 
                           m.args)
    report.mod.form <- unlist(lapply(all.mods, 
                                     function(x) x$formula))
    report.mod.r2 <- unlist(lapply(all.mods, 
                                     function(x) x$r2))
    refs <- plot.args$group.ids %in% 
               unique(names(report.mod.form), 
                      names(report.mod.r2))
    lines <- list(col=c(NA, plot.args$col[refs]),
                  lty=c(1, plot.args$lty[refs]),
                  lwd=c(1, plot.args$lwd[refs]),
                  size=0.9)
    formulas <- c("Fit", report.mod.form)
    r2 <- c(NA, report.mod.r2)
    r2[1] <- expression(paste("(", R^2, ")",sep=""))
    key.gf <- draw.key(list(lines=lines,
                    text=list(formulas,
                              cex=0.65),
                    text=list(r2, cex=0.65),
                between =0.9, padding.text=1,
                border=1, background="white"), 
                draw = FALSE)
    #key.gf <- draw.key(list(lines=TRUE), draw=FALSE)
    vp <- viewport(x = unit(m.args$position[1], "npc") + 
                     unit(0.5 - m.args$position[1], 
                     "grobwidth", list(key.gf)), 
                   y = unit(m.args$position[2], "npc") + 
                      unit(0.5 - m.args$position[2], 
                      "grobheight", list(key.gf)))
    pushViewport(vp)
    grid.draw(key.gf)
    upViewport()
  }
}


#cols <- colHandler(1:200, col.regions="Blues")[101:200]
#p <- loaPlot(~Temp*Ozone|Temp>80, cex=0.5,
#        groups=airquality$Temp>70, alpha=0.8,
#        data=airquality, col.regions="Spectral")
#p <- loaPlot(~Temp*Ozone, cex=0.5,
#        data=airquality)
#update(addXYLOESSFit(p), 
#       ylim=c(-35,175), xlim=c(55, 98))
#p <-addXYLMFit(p)
#(p$panel.args.common$loa.mod.fit)


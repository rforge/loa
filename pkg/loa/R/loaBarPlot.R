#indevelopment

#loaBarPlot
#

################
################
##loaBarPlot
################
################


##################################
#to do
##################################
#new key with better top and bottom options
#


#######################
#to think about
#######################
#look at x,y, group, cond input cf with pemsInput?
#look at x,y, group, cond handling?
#make na handling specific to x, y, group, cond?
#allow it to draw horizontal bar 
#    with y as factor and x as calculation case...
########################
#think this falls over if multiple cond or group 
#    arguments are applied


#kr 31/05/2017
#ver 0.1

#kr 29/06/2017
#ver 0.2 (now .old)





loaBarPlot <- function(x, y=NULL, groups=NULL, cond=NULL, data=NULL, ..., 
              drop.nas=TRUE, stat=NULL){

#test
#require(loa)
#require(pems.utils)
#require(plyr)

      argnames <- names(as.list(match.call(expand.dots = TRUE)[-1]))
      arguments <- as.list(match.call()[-1])

      extra.args <- list(...)
      if(!is.null(data)) data <- as.data.frame(data)
      env <- parent.frame()

      if("formula" %in% class(try(x, silent=TRUE))){

          #if x is formula   
          fm <- formulaHandler(x, data=data, formula.type="y~x|cond", output="lattice.like")
          x.name <- if(is.null(fm$xlab)) character() else fm$xlab
          y.name <- if(is.null(fm$zlab)) character() else fm$zlab
          groups.name <- as.character(arguments)[argnames == "groups"]
          cond.name <- if(is.null(fm$panel.condition)) character() else 
                             paste(names(fm$panel.condition),sep="+")
          df <- list(x = fm$x, y = fm$z, 
                     groups = eval(substitute(groups), data, env), 
                     cond = fm$panel.condition[[1]])
          
      } else {

         x.name <- as.character(arguments)[argnames == "x"]
         y.name <- as.character(arguments)[argnames == "y"]
         groups.name <- as.character(arguments)[argnames == "groups"]
         cond.name <- as.character(arguments)[argnames == "cond"]
         df <- list(x = eval(substitute(x), data, env), 
                    y = eval(substitute(y), data, env), 
                    groups = eval(substitute(groups), data, env), 
                    cond = eval(substitute(cond), data, env))

      }

#here you have x.name, etc as character() or name
#df as list of x,y,groups,cond with empties as NULL...

      ref <- lapply(df, length)

      #ref is length not is.null so we can see that vector fit
      ## length ==0 is null...

      if(is.null(stat)){
          if(ref$y==0){
                 y.name <- "count"
                 stat <- function(y) length(y)
          } else {
                 stat <- function(y) sum(y, na.rm=TRUE)
      }}

###################
#something to look into
#this falls over from as.data.frame.list 
#takes name from name in attributes??
#so is name of e.g. y instead of y
##      df <- as.data.frame(df[ref>0])
###################
#temp fix force names 
     ref <- names(ref)[ref > 0]
     df <- as.data.frame(df[ref])
     names(df) <- ref
###################

      if(is.null(df$y))
          df$y <- rep(0, length(df$x))

#####################
#added next three to catch NAs...
#in x
#in groups and cond
#####################

      if(!is.null(df$x) && !is.factor(df$x))
            df$x <- if(drop.nas) factor(df$x) else
                              factor(df$x, exclude=FALSE)

      if(!is.null(df$groups) && !is.factor(df$groups))
            df$groups <- if(drop.nas) factor(df$groups) else
                              factor(df$groups, exclude=FALSE)

      if(!is.null(df$cond) && !is.factor(df$cond))
            df$cond <- if(drop.nas) factor(df$cond) else
                              factor(df$cond, exclude=FALSE)


      sum.df <- ddply(df, names(df)[names(df) !="y"], function(df) stat(df$y), .drop=drop.nas)
      if(drop.nas){
          sum.df <- na.omit(sum.df)
      } else {
         for(i in names(sum.df))
             if(is.factor(sum.df[,i]))
                  sum.df[,i] <- factor(sum.df[,i], levels(df[,i]), exclude=FALSE)
      }

########################
#think about this re multi-return functions...
      names(sum.df)[names(sum.df)=="V1"] <- "y"
########################

      form <- if("y" %in% names(sum.df)) "y~" else "~"
      form <- if("x" %in% names(sum.df)) paste(form, "x", sep="") else form
      form <- if("cond" %in% names(sum.df)) paste(form, "|cond", sep="") else form

      form <- as.formula(form)

      #col mapping
      grps <- if(is.null(sum.df$groups)) 1 else if(is.factor(sum.df$groups)) levels(sum.df$groups) else sort(unique((sum.df$groups)))
####################
#tested update below
#      temp <- colHandler(1:100, col.regions="Blues")[c(20,80)]
      temp <- colHandler(1:20, col.regions="Spectral")
####################
      temp <- listUpdate(listUpdate(list(col.regions=temp),extra.args), list(z=1:length(grps)), ignore=c("zlim"))
####################
#test
#replace    col <- do.call(colHandler, temp)
#so we get blue as single col...
#could move to colHandler as option?
       col <- if(length(temp$z)<2) {
           temp$z <- c(1,2)
           do.call(colHandler, temp)[2]
       }  else do.call(colHandler, temp)  
#
 
      #strip col args in case they come back to bite us...

      extra.args <- extra.args[!names(extra.args) %in% c("alpha", "alpha.regions", "col.regions")]
 
      plot.list <- list(x=form, groups=sum.df$groups, data=sum.df, stack=TRUE, 
                        origin=0, xlab=x.name, ylab=y.name, col=col, 
#                        key = list(space="top", adj=1,
#                                   text = list(grps),
#                                   rect = list(col=col)),
                        panel = function(...){
                                 panel.grid(-1,-1)
                                 panel.barchart(...)
                        })

##############
#this is messy but needed so any NAs in x are shown if requested
#must be a better way...
##############

      if(!drop.nas){
           temp <- plot.list$data$x
           if(is.factor(temp)){
              temp <- as.character(temp)
              if(any(is.na(temp))){
                  temp[is.na(temp)] <- "NA"
                  plot.list$data$x <- factor(temp, c(levels(plot.list$data$x), "NA"))
              }
           }
      }

      extra.args <- do.call(scalesHandler, extra.args)
      extra.args <- listUpdate(plot.list, extra.args)

      plt <- do.call(barchart, extra.args)

      if(!is.null(sum.df$groups)){
############################
#key
#should be nicer
############################
            if("stack" %in% names(extra.args) && !extra.args$stack){
                grps <- rev(grps)
                col <- rev(col)
            }

#            temp <- list(fun="draw.key",
#                         args=list(key=list( 
#                                   space="right", adj=1,
#                                   title=groups.name, 
#                                   text=list(as.character(grps)),
#                                   rect=list(col=col), 
#                                   rep=FALSE), 
#                         draw=FALSE))
#tider key using own function

            temp <- list(fun="draw.zcasePlotKey",
                         args=list(key=list( 
                                   space="right", adj=1,
                                   zcases.main=if("key.main" %in% names(extra.args)) 
                                                   extra.args$key.main else groups.name, 
                                   zcase.ids=as.character(grps),
                                   col=col), 
                         draw=FALSE))
            plt$legend$right <- temp           
      }


     plt$loa <- list(raw.data = df, results=sum.df)

################################
#only need this if we are reworking plot

     plt
}










###############################
#unexported
###############################


#kr 10/01/2021 
#ver 0.3

loaBarPlot.new <- function (x, data = NULL, ...){
   
   extra.args <- list(...)
   extra.args <- listUpdate(list(x = x, data = data, formula.type = "z~x|cond", 
                                 #coord.conversion = XZ2XYZ, 
                                 panel = panel.loa), 
                            extra.args)
   
   do.call(loaPlot, extra.args)
}

XZ2XYZ <- function(col=NULL, col.regions=NULL, par.settings=NULL, scheme=NULL, border=NULL, key.handling = NULL, force.key=NULL,...){
   
   #this is from stackPlot
   
   #set up
   extra.args <- list(...)
   
   #y data and labels
   extra.args$y <- extra.args$z
   if(!"ylab" %in% names(extra.args))
      extra.args$ylab <- extra.args$zlab
   
   #defualt one col
   cols <- 1
   
   if(!"zcases" %in% names(extra.args)) {
      zcases <- rep("default", length(extra.args$y))
      zcase.ids <- "default"
   } else {
      zcases <- extra.args$zcases
      zcase.ids <- extra.args$zcase.ids
      cols <- 1:length(zcase.ids)
   }
   
   if("panel.condition" %in% names(extra.args)){
      zcases <- extra.args$panel.condition$zcases
      zcase.ids <- unique(extra.args$panel.condition$zcases)
   }
   
################################
#this is stacking code
   
#   ref.x <- extra.args$x[zcases == zcase.ids[1]]
#   ref.y <- rep(0, length(ref.x))
#   extra.args$y0 <- extra.args$y
   
#   for(i in zcase.ids){
#      temp <- extra.args$y[zcases == i]
#      temp <- temp - min(temp, na.rm=T) #range[1] might be more robust?
#      temp[is.na(temp)] <- 0
#      extra.args$y[zcases == i] <- temp + ref.y
#      extra.args$y0[zcases == i] <- ref.y
#      if(!"panel.condition" %in% names(extra.args)) ref.y <- ref.y + temp
#   }
   

###############################
   
   #this is the cheat to recolour the plot
   #this works because it is only applied to 
   #in the colHandler call below
   
   if(is.null(scheme))
      scheme <- "kr.web"
   
   #need to look into why include par.settings = NULL 
   #stop colHandler looking at scheme
   
   extra.args$col <- colHandler(z=cols, ref=cols, col=col, col.regions=col.regions, scheme=scheme)
   if(is.null(border))
      extra.args$border <- extra.args$col
   
   #this is the cheat for the new key handling 
   
   if(is.null(force.key))
      extra.args$ycase.key.method2 <- TRUE
   
   extra.args
}


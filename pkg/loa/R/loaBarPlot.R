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
#ver 0.2


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

      df <- as.data.frame(df[ref>0])

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

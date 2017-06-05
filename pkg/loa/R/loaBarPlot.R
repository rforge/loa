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
#DONE reverse order of key when stack=TRUE
#DONE Use Spectral as default col
#DONE default key to right
#
#BUT 
#might want key at top when not stacking???
#might want to rethink spectral when only one 
#that is yellow...
#


#kr 31/05/2017
#ver 0.1


loaBarPlot <- function(x, y=NULL, groups=NULL, cond=NULL, data=NULL, ..., 
              stat=NULL){

#test
#require(loa)
#require(pems.utils)
#require(plyr)

      argnames <- names(as.list(match.call(expand.dots = TRUE)[-1]))
      arguments <- as.list(match.call()[-1])

      extra.args <- list(...)

      #var.names
      x.name <- as.character(arguments)[argnames=="x"]
      y.name <- as.character(arguments)[argnames=="y"]
      groups.name <- as.character(arguments)[argnames=="groups"]
      cond.name <- as.character(arguments)[argnames=="cond"]

#print(argnames)
#print(argnames=="x")
#print(as.character(arguments))
#print(x.name)

      #name is character(0) if not declared

      if(!is.null(data)) data <- as.data.frame(data)
      env <- parent.frame()

      #when packaged this might be 
      #here <- x
      ##x <- eval(substitute(x), data, environment(here))

      df <- list(x = eval(substitute(x), data, env),
                 y = eval(substitute(y), data, env),
                 groups = eval(substitute(groups), data, env),
                 cond = eval(substitute(cond), data, env))

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

      sum.df <- ddply(df, names(df)[names(df) !="y"], function(df) stat(df$y))

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
      col <- do.call(colHandler, temp)
 
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

      extra.args <- listUpdate(plot.list, extra.args)

      plt <- do.call(barchart, extra.args)

      if(!is.null(sum.df$groups)){
############################
#key
#should be nicer
############################
            if("stack" %in% names(extra.args) && extra.args$stack){
                grps <- rev(grps)
                col <- rev(col)
            }

            temp <- list(fun="draw.key",
                         args=list(key=list( 
                                   space="right", adj=1,
                                   title=groups.name, 
                                   text=list(as.character(grps)),
                                   rect=list(col=col), 
                                   rep=FALSE), 
                         draw=FALSE))
            plt$legend$right <- temp           
      }


     plt$loa <- list(raw.data = df, results=sum.df)

################################
#only need this if we are reworking plot

     plt
}

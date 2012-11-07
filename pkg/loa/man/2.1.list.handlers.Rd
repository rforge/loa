\name{2.1.list.handlers}
\alias{2.1.list.handlers}
\alias{listHandler}
\alias{listUpdate}
\alias{listExpand}

\title{
  List manipulation
}

\description{
  Workhorse functions for routine list handling in loa and elsewhere.
}
\usage{

listHandler(a, use = NULL, ignore = NULL, 
            drop.dots=TRUE)

listUpdate(a, b, use = NULL, ignore = NULL,
            use.a = use, use.b = use,
            ignore.a = ignore, ignore.b = ignore, 
            drop.dots = TRUE)

listExpand(a, ref = NULL, use = NULL, 
            ignore = NULL, drop.dots = TRUE)

}

\arguments{

  \item{a}{Required. The list to be modified.
  } 
  \item{b}{(For \code{listUpdate}) A required second list, the contents of 
   which are used to update \code{a} with.
  } 
  \item{use, use.a, use.b}{ Vectors, all defaults \code{NULL}. If supplied, 
   a vector of the names of list enteries to be used. Other enteries are 
   then discarded. \code{use} is applied to all supplied lists, while 
   \code{use.a}, \code{use.b}, etc. can be used to subset \code{a} and 
   \code{b} lists individually.  
  } 
  \item{ignore, ignore.a, ignore.b}{ Vectors, default \code{NULL}. As with 
   \code{use}, etc, but for enteries to be ignored/not passed on for 
   modification.  
  } 
  \item{ref}{(For \code{listExpand}) A vector, default \code{NULL}. A reference 
   data series, the length of which is used as the expansion length to be 
   applied when wrapping of list enteries.  
  } 
  \item{drop.dots}{logical, default \code{TRUE}. If \code{TRUE}, this 
   removes \code{"..."} entries from lists before updating. 
  } 

}


\details{
  \code{listHandler} is a general function used by other \code{list...} 
   functions for routine list preprocessing.

  \code{listUpdate} is a list handler intended for use when managing 
   user updates for default options (see examples).

  \code{listExpand} is a list handler that expands vectors to 
   a given reference length, intended for use for data wrapping. 
     
}

\value{
  All \code{list...} functions return a list of the modified (or 
  updated) elements of supplied list \code{a}. 
}
\references{
  These functions make extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5
}

\author{
  Karl Ropkins
}

\note{
  IN DEVELOPMENT: Function structures may change in future 
  package updates.
}

\seealso{
   \code{\link{lattice}}, \code{\link{xyplot}}, 
}

\examples{

##example 1
##general

#two lists
list1 <- list(a = 1:10, b = FALSE)
list2 <- list(b = TRUE, c = "new")

#updating a with b
#keeps unchanged list1 entry, a
#updates changed list1 entry, b
#adds new (list2) entry, c
listUpdate(list1, list2)


##example2
##use in plot function

#function
myplot <- function(x, ...){

    #defaults I set for myplot form of xyplot
    mylist <- list(x = x, pch = 20, col = "red",
                   panel = function(...){
                           panel.grid(-1, -1)
                           panel.xyplot(...)
                           panel.abline(0,1)
                   })
                   
    #what user did/didn't update/add
    extra.args <- list(...)
   
    #update mylist
    plotlist <- listUpdate(mylist, extra.args)

    #plot
    do.call(xyplot, plotlist)
}

a <- 1:10
b <- rnorm(10,5,2)

myplot(a~b)
myplot(a~b, col = "blue", xlim=c(1:20)) 

##NOTE

#if myplot had been coded more simply
#e.g.
badplot <- function(x, ...){

    #setting defaults in xyplot call itself
    xyplot(x = x, pch = 20, col = "red",
                   panel = function(...){
                           panel.grid(-1, -1)
                           panel.xyplot(...)
                           panel.abline(0,1)
                   }, ...)
}

badplot(a~b) #OK
badplot(a~b, xlim=c(0,10)) #OK

#BUT would fail on
#badplot(a~b, col= "blue") 
#because col hardcoded into call
#AND duplicated in call and '...'
#so user cannot update

}


\keyword{ methods }

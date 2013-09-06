\name{4.8.list.handlers}
\alias{4.8.list.handlers}
\alias{listHandler}
\alias{listUpdate}
\alias{listExpand}
\alias{listLoad}

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

listLoad(..., load = NULL)

}

\arguments{

  \item{a}{ A required list. The list to be modified.
  } 
  \item{b}{ For \code{listUpdate} only, a required second list, the contents of 
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
  \item{ref}{For \code{listExpand} only, a vector, default \code{NULL}. A reference 
   data series, the length of which is used as the expansion length to be 
   applied when wrapping of list enteries.  
  } 
  \item{drop.dots}{ Logical, default \code{TRUE}. If \code{TRUE}, this 
   removes \code{"..."} entries from list names before updating. 
  } 
  \item{...}{ For \code{listLoad} only, any additional arguments.  
  } 
  \item{load}{ For \code{listLoad}) only, a vector, default \code{NULL}. The names 
   of any lists to be automatically generated from the additional arguments supplied 
   as part of the command call.  
  } 

}


\details{
  \code{listHandler} is a general function used by other \code{list...} 
   functions for routine list preprocessing.

  \code{listUpdate} is a list handler intended for use when managing 
   user updates for default options (see examples).

  \code{listExpand} is a list handler that expands vectors to 
   a given reference length, intended for use for data wrapping. 

  \code{listLoad} is an IN-DEVELOPMENT list generator. See Note below.
     
}

\value{
  By default, all \code{list...} functions return results as lists.

  \code{listHandler}, \code{listUpdate} and \code{listExpand} functions 
  all return a modified (or updated) version of supplied list \code{a}. 

  \code{listLoad} (in-development) returns modified (or updated) version 
  of additional arguments as a list. See Note below.
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
  IN DEVELOPMENT: \code{listLoad} is an in-development workhorse function 
  that generates lists based on the supplied \code{load} argument. 

  It assumes each element of \code{load} is the name of an expected list 
  and searches the associated additional arguments for arguments to 
  populate it with using the rule '\code{[load].[arg]} is an element of list 
  \code{[load]}'. So, for example, for a call including the arguments 
  \code{load = 'key'} and \code{key.fun = draw.colorkey}, it would strip out 
  both arguments and return \code{key = list(fun=draw.colorkey)}. Used in 
  functions in series it allowed list-in-list that can be commonplace 
  when modifying, for example, key elements in conventional \code{lattice} 
  plots to be simplified. 

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
##use in plot functions
##to simplify formals

#some data

a <- 1:10
b <- rnorm(10,5,2)

#a bad plot function

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
#because col hardcoded into badplot function
#AND duplicated in call and '...'
#so user cannot update col 


#a standard correction

stdplot <- function(x, pch = 20, col = "red", ...){

    #setting defaults in xyplot call itself
    xyplot(x = x, pch = 20, col = "red",
                   panel = function(x=x, pch=pch, col=col, ...){
                           panel.grid(-1, -1)
                           panel.xyplot(x=x, pch=pch, col=col, ...)
                           panel.abline(0,1)
                   }, ...)
}

stdplot(a~b)
stdplot(a~b, col = "blue", xlim=c(1:20))
 

#an alternative correction using lists and 
#listUpdate that removes the need for formal 
#definition of all modified plot arguments 

myplot <- function(x, ...){

    #defaults I set for myplot form of xyplot
    mylist <- list(x = x, pch = 20, col = "red",
                   panel = function(...){
                           panel.grid(-1, -1)
                           panel.xyplot(...)
                           panel.abline(0,1)
                   })
    #plot
    do.call(xyplot, listUpdate(mylist, list(...)))
}


myplot(a~b)
myplot(a~b, col = "blue", xlim=c(1:20)) 


}


\keyword{ methods }

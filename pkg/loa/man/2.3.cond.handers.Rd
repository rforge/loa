\name{2.3.cond.handlers}
\alias{2.3.cond.handlers}
\alias{2.3.conditioning.handlers}
\alias{condsPanelHandler}
\alias{groupsPanelHandler}
\alias{zcasesPanelHandler}
\alias{groupsAndZcasesPanelHandler}
\alias{groupsHandler}
\alias{zcasesHandler}
\alias{groupsAndZcasesHandler}



\title{
  Plot Conditioning
}
\description{
  Plot conditioning handling
}
\usage{

  condsPanelHandler(..., conds = NULL, panel = NULL, 
         by.cond = NULL, process = TRUE, plot = TRUE)

  groupsPanelHandler(..., groups = NULL, panel = NULL, 
         by.group = NULL, process = TRUE, plot = TRUE)

  zcasesPanelHandler(..., zcases = NULL, panel = NULL, 
         by.zcase = NULL, process = TRUE, plot = TRUE)

  groupsAndZcasesPanelHandler(panel=NULL, ..., 
         plot = TRUE, process = TRUE)  


  groupsHandler(z = NULL, groups = NULL, ..., group.ids = NULL, 
         handler = "zzz")

  zcasesHandler(z = NULL, zcases = NULL, ..., zcases.ids = NULL, 
         handler = "zzz")

  groupsAndZcasesHandler(..., loa.settings = NULL)

}

\arguments{

  \item{...,}{ Additional arguments. See Notes below.
   }

  \item{conds, panel, by.cond}{ For all supplied additional arguments, 
   \code{conds} is a vector of conditioning indices. This is 
   typically a logical, numeric, factor or character vector which can 
   be used to assign other elements undeclared call arguments to specific 
   subsets. \code{panel} identifies the \code{panel...} function, and 
   should also be supplied so loa can manage processing and plot activities 
   correctly. \code{by.cond} identifies routine plot operations associated 
   with the requested conditioning. This can be a list of plot arguments or 
   \code{panel...} functions that should be associated with the requested 
   conditoning. See \code{process} and \code{plot} below and associated Notes.

   }

   \item{plot, process}{Passed to and handled by \code{\link{panelPal}}. For panels that can be 
   preprocessed, these turn off or one processing and the plotting steps of the panel code. See 
   \code{\link{panelPal}} Help documentation from further details. 
   }


  \item{groups, by.group}{ As \code{conds} and \code{by.cond} but for 
   grouping. 
   }

  \item{zcases, by.zcases}{ As \code{conds} and \code{by.cond} but for 
   zcase condition. 
   }

  \item{z, group.ids, handler, zcases.ids, loa.settings}{[TO BE DOCUMENTED]
  }

}

\details{
  NOTE: These functions are currently in development and may be subject to changes.

  \code{condsPanelHandler} is a general purpose function that can be used to routinely 
  manage plot conditioning within a \code{panel...} function call. It takes all undeclared 
  arguments are supplied to it, and subsets them by unique case in the supplied \code{conds} 
  argument. Then it modifies each of these based on the associated elements of \code{by.cond} 
  and processes and/or plots the results depending on \code{process} and \code{plot} settings.

  \code{groupsPanelHandler} is similar but is intended for use with the plot call argument 
  \code{groups}.

  \code{zcasesPanelHandler} is similar but is intended for use with arguments conditioned 
   within the z term of the plot formula. So, for example, for unique handling of \code{z1} 
   and \code{z2} cases in the plot \code{loaPlot(z1+z2~x*y)}.

  \code{groupsAndZcasesPanelHandler} is a wrapper for \code{groups} and \code{zcase} that 
  allows users to simultaneously and uniquely handle both types of conditioning. 
 
}

\value{

  All \code{..PanelHandlers} functions are intended for use with \code{\link{panelPal}}. 
  Using different combinations of these allows plot developers a high degree of 
  flexibility. 

  [DOCUMENT FURTHER]

   
}

\note{

  This function is in development and may be subject to changes.
     
}


\references{
  This function makes extensive use of code developed by others.  

  lattice:
  Sarkar, Deepayan (2008) Lattice: Multivariate Data
  Visualization with R. Springer, New York. ISBN
  978-0-387-75968-5

}

\author{
  Karl Ropkins
}

\seealso{

   \code{\link{panelPal}}

   For information on related functions in other packages, see

   \code{\link{lattice}}: \code{\link{xyplot}}; \code{\link{panel.xyplot}}; and 
   \code{\link{panel.levelplot}}.

}

\examples{
#TO BE CONFIRMED


}

\keyword{ methods }

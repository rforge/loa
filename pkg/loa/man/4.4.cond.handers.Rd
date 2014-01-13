\name{4.4.cond.handlers}
\alias{4.4.cond.handlers}
\alias{4.4.conditioning.handlers}
\alias{condsPanelHandler}
\alias{groupsPanelHandler}
\alias{zcasesPanelHandler}
\alias{groupsAndZcasesPanelHandler}
\alias{groupsHandler}
\alias{zcasesHandler}
\alias{groupsAndZcasesHandler}
\alias{stepwiseZcasesGlyphHandler}



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

  stepwiseZcasesGlyphHandler(zcases = NULL, ..., zcase.ids = NULL, 
         panel.elements = NULL, loaGlyph = NULL)

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

   \item{plot, process, loa.settings}{Passed to and handled by \code{\link{panelPal}}. 
   For panels that can be preprocessed, \code{plot} and \code{process} turn off or 
   on processing and the plotting steps of the panel code. See \code{\link{panelPal}} Help 
   documentation from further details. 
   }


  \item{groups, by.group}{ As \code{conds} and \code{by.cond} but for 
   grouping. 
   }

  \item{zcases, by.zcase}{ As \code{conds} and \code{by.cond} but for 
   zcase condition. 
   }

  \item{z, handler}{ The \code{z} data series and any associated plot arguments that 
   need special handling.
  }

  \item{group.ids, zcases.ids, zcase.ids}{ If given, vectors of the unique cases in 
   \code{groups} and \code{zcases}, respectively.
  }

  \item{panel.elements}{ If given, the names of all plot arguments that have been 
   vectorized by \code{panelPal}.
  }

  \item{loaGlyph}{ (For \code{stepwiseZcasesGlyphHandler} only), the loa glyph to 
   drawn. See \code{\link{loa.glyphs}} for further details.
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

  \code{stepwiseZcasesGlyphHandler} is a \code{...Handler} function for generating gylph plots 
  based on multiple z inputs. 
 
}

\value{

  All \code{..PanelHandlers} functions are intended for use with \code{\link{panelPal}}. 
  Using different combinations of these allows plot developers a high degree of 
  flexibility. 
   
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

%examples to do
%\examples{
%#TO BE CONFIRMED
%}

\keyword{ methods }

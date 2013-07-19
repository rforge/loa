\name{3.1.example.data}
\alias{3.1.example.data}
\alias{example.data}
\alias{lat.lon.meuse}


\docType{data}

\title{
   example data for use with loa
}

\description{
   Example data intended for use with examples in \code{loa}. 
}

\usage{

   lat.lon.meuse

}


\format{
  \code{lat.lon.meuse} is a modified form of the \code{meuse} data set 
   taken from the \code{sp} package. Here, coordinate (x,y) elements 
   have been transformed to latitudes and longtiudes and the object 
   class has been converted from \code{SpatialPointsDataFrame} to 
   \code{data.frame}. 
     
}

\details{
  \code{lat.lon.meuse} was generated using method based on \code{mzn} object 
  production in \url{https://github.com/etes/Geoprocessing/blob/master/heatmap.R}.

\code{ library(sp); library(gstat); library(rgdal)} \cr
\code{ data(meuse) } \cr 
\code{ coordinates(meuse) =~ x + y } \cr 
\code{ proj4string(meuse) = CRS("+init=epsg:28992") } \cr      
\code{ meuse1 = spTransform(meuse, CRS("+init=epsg:4326")) } \cr   
\code{ meuse2=as.data.frame(meuse1) } \cr   
\code{ mzn=meuse2[,c(14,13,4)] } \cr   
\code{ names(mzn)<-c("Latitude","Longitude","zinc") } \cr   

}

\note{
  None at present
}

\source{
  Reference in preparation
}

\references{

For \code{meuse}:

M G J Rikken and R P G Van Rijn, 1993. Soil pollution with heavy metals - an inquiry 
into spatial variation, cost of mapping and the risk evaluation of copper, cadmium, 
lead and zinc in the floodplains of the Meuse west of Stein, the Netherlands. 
Doctoraalveldwerkverslag, Dept. of Physical Geography, Utrecht University 

P.A. Burrough, R.A. McDonnell, 1998. Principles of Geographical Information Systems. 
Oxford University Press. 

Stichting voor Bodemkartering (STIBOKA), 1970. Bodemkaart van Nederland : Blad 59 Peer, 
Blad 60 West en 60 Oost Sittard: schaal 1 : 50 000. Wageningen, STIBOKA. 

For \code{sp}:

Roger S. Bivand, Edzer J. Pebesma, Virgilio Gomez-Rubio, 2008.
Applied spatial data analysis with R. Springer, NY.
http://www.asdar-book.org/

Pebesma, E.J., R.S. Bivand, 2005. Classes and methods for spatial
data in R. R News 5 (2), http://cran.r-project.org/doc/Rnews/.

}

\examples{

#data structure

head(lat.lon.meuse)

#various plots

loaPlot(~longitude*latitude, data=lat.lon.meuse)

loaPlot(cadmium~longitude*latitude, data=lat.lon.meuse)

loaPlot(cadmium~longitude*latitude, col.regions=c("lightblue", "darkblue"), 
        data=lat.lon.meuse)

loaPlot(cadmium+copper+lead+zinc~longitude*latitude, panel.zcases = TRUE, 
        col.regions=c("lightblue", "darkblue"), 
        key.z.main="Concentrations", data=lat.lon.meuse)


}

\keyword{datasets}

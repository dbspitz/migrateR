\name{spatmig}
\alias{spatmig}
\title{ Spatial Comparison of Movement Models}
\description{ Groups location based on parameter estimates from selected \code{mvmt} model}
\usage{
spatmig(ltraj, mvmt, mod = "migant", graph = F, new = F)
}
\arguments{
  \item{ltraj}{object of class \code{\link{ltraj}}}
  \item{mvmt}{ object of class \code{mvmt} or \code{mvmts}, as from the output of \code{\link{mvmtClass}}}
  \item{mod}{ model to use in categorizing points ("disperser", "migrant" or "mixmig") }  
  \item{graph}{ produce plot?}
  \item{new}{ call new device for plot? }
}  
\details{
Identifies each location as either belonging to the starting range (range 1), a secondary range (range 2) or as remaining unclassified. Locations with t < theta and NSD < mean(NSD) are assigned to range 1. If the migrant model is used, locations with t > theta2 and NSD < mean(NSD) are also assigned to this range. Locations with theta < t < theta2 and NSD > mean(NSD) are assigned to range 2. All remaining points (i.e. those belonging to neither range 1 nor range 2) are identified as unclassified.
For \code{typ = "elev"}  models, groupings are equivalent, with elevation values substituted for NSD. However, if the migration is downward (i.e. if delta < 0) the inequalities used with elevation are flipped (such that range 1 only includes locations wih elev > mean(elev) and range 2 includes locations with elev < mean(elev)).  
}
\value{
Returns a vector corresponding to the point classifications indicating whether a point belongs to range 1 (1), range (2) or remained unclassified (NA).
If \code{graph = T}, \code{mvmt} and spatial plots are shown side by side with each location's grouping identified on both plots by ploting character. In the spatial plot (right panel) the first location (or rdt, if supplied) is emphasized with a box outlined in black.
}
\author{ Derek Spitz }
\seealso{
\code{\link{mvmtClass}}
}
\examples{

	# Fit "NSD" and "elev" models to bighorn trajectory 3 
	bh3nsd<-mvmtClass(bighorn[2], p.est = pEst(s.d = 10))
	bh3elev <-mvmtClass(bighorn[2], typ = "elev", stdt = "10-31", p.est= pEst(s.t=15))

	# Compare:
	spatmig(bighorn[2],bh3nsd,graph=T)
	
	spatmig(bighorn[2],bh3elev,graph=T)

}

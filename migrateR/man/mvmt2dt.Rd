\name{mvmt2dt}
\alias{mvmt2dt}
\title{ Estimate start and end time of migratory or dispersal movement }
\description{ This function returns parameter estimates from non-linear animal movement, grouped into data.frames by model type.}
\usage{
mvmt2dt(mvmt, p = 0.05, mod = "migrant")
}
\arguments{
  \item{mvmt}{ object of class \code{mvmt} or \code{mvmts}, as from the output of \code{\link{mvmtClass}}}
  \item{p}{numeric: threshold fraction of total distance moved}
  \item{mod}{character: movement model chosen for estiamtes, either "disperser", "migrant" or "mixmig"}
}  
\details{
The value of \code{p} must be greater than zero and no greater than 0.5 (when p=0.5, the function returns estimates of theta and, for migrants and mixed migrants, theta2).
}
\value{
Returns a data.frame for each "mvmt" in \code{mvmt} including the start and end dates of dispersal or migratory movements. These dates are provided as dates of class "POSIXct" and as decimal days (calculated from \code{stdt}, see \code{\link{mvmtClass}}) to facilitate comparison across years. If \code{mod} is missing from any elements of \code{mvmt}, "NULL" is returned instead for these elements.
}
\references{
Bunnefeld, N., L. Borger, B. van Moorter, C. M. Rolandsen, H. Dettki, E. J. Solberg, and G. Ericsson. (2011) A model-driven approach to quantify migration patterns: individual, regional and yearly differences. Journal of Animal Ecology 80, 466-476.

Spitz, D. (2015). Does Migration Matter? Causes and Consequences of Migratory Behavior in Sierra Nevada Bighorn Sheep. Dissertation thesis, University of Montana.
}
\author{
 D. Spitz
}
\seealso{
\code{\link{mvmtClass}}
\code{\link{topmvmt}}
}
\examples{
  data(elk)

  elkh <- head(elk)  # subset (for speed)

  elk.nsd <- mvmtClass(elkh,stdt="3-1",p.est=pEst(s.d=500))

  elk.nsd

  mvmt2dt(elk.nsd)
}

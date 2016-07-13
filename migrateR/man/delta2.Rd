\name{delta2}
\alias{delta2}
\title{Calculate Timing of Return Migration}
\description{ This function derives an estimate of the return distance traveled by mixed migrants  parameters estimated by \code{mvmtClass}. }
\usage{
delta2(mvmt)
}
\arguments{
  \item{mvmt}{ object of class \code{mvmt} or \code{mvmts}, as from the output of \code{\link{mvmtClass}}}
}
\details{
Calculates the derived paramter delta2 = delta*zeta with a call to \code{deltaMethod}. For a further discussion of NSD models and their parameterization, see \code{\link{mvmtClass}} and Bunnefeld et al. 2011.
}
\value{
Returns a data.frame containing estimates and associated standard errors. If the mixed migrant model ("mixmig") is missing from any elements of \code{mvmt}, "NA" is returned instead for these elements.

}
\references{
Bunnefeld, N., L. Borger, B. van Moorter, C. M. Rolandsen, H. Dettki, E. J. Solberg, and G. Ericsson. (2011) A model-driven approach to quantify migration patterns: individual, regional and yearly differences. Journal of Animal Ecology 80, 466-476.

Spitz, D. (2015). Does Migration Matter? Causes and Consequences of Migratory Behavior in Sierra Nevada Bighorn Sheep. Dissertation thesis, University of Montana.
}
\author{ Derek Spitz }
\seealso{
\code{\link{mvmtClass}}
\code{\link{topmvmt}}
\code{\link{deltaMethod}}
}
\examples{
data(elk)

elkh <- head(elk)  # subset (for speed)

elk.nsd <- mvmtClass(elkh,stdt="3-1",p.est=pEst(s.d=500))

elk.nsd

delta2(elk.nsd)
}

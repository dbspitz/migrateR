\name{theta2}
\alias{theta2}
\title{Calculate Timing of Return Migration}
\description{ This function derives an estimate of the midpoint in time of return migratory movement from the timing parameters estimated by \code{mvmtClass}. }
\usage{
theta2(mvmt,mod="migrant")
}
\arguments{
  \item{mvmt}{ object of class \code{mvmt} or \code{mvmts}, as from the output of \code{\link{mvmtClass}}}
  \item{mod}{ character: the model from which theta2 should be derived, either "migrant" or "mixmig"}
}
\details{
Calculates the derived paramter theta2 = theta + 2*phi + rho + 2*phi2 with a call to \code{deltaMethod}. For \code{mvmt} objects containing the "nsd" family of models, setting the argument \code{mod="mixmig"} allows calculation of theta2 from the mixed-migrant model. For a further discussion of these models and their parameterization, see \code{\link{mvmtClass}} and Bunnefeld et al. 2011.
}
\value{
Returns a data.frame containing estimates and associated standard errors.  If a burst is missing the required model, "NA" is returned instead.
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

elk.nsd <- mvmtClass(elkh,stdt="3-1")

elk.nsd

theta2(elk.nsd)
}

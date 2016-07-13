\name{mvmtClass}
\alias{mvmtClass}
\alias{mvmtNSD}
\alias{mvmtElev}
\title{Fit Movement Models to Animal Location Data}
\description{This function attempts fit of a family of non-linear animal-movement models to location data.}
\usage{

mvmtClass(ltraj, fam = "nsd", p.est = pEst(), stdt = NA, scut = NA, ecut = NA, rloc = 1, warnOnly = F)
}
\arguments{
  \item{ltraj}{object of class \code{\link{ltraj}}}
  \item{fam}{family of models to be fit, either "nsd","nsd3d", "nsd3d.std" or "elev"}
  \item{p.est}{starting parameter estimates and constraints, as provided by \code{\link{pEst}}}
  \item{stdt}{date from which migratory timing (theta, \code{\link{theta2}})is estimated}
  \item{scut}{exclude values before date, character formatted as "\%m-\%d"}
  \item{ecut}{exclude values after date, character formatted as "\%m-\%d"}
  \item{rloc}{numeric: reference locaiton from which rNSD is calculated}
  \item{warnOnly}{logical: should results include models that triggered convergence warnings?}
}
\details{
The family of models fit depends on the \code{fam} argument. For "nsd", "nsd3d" and "nsd3d.std", the following five models are fit:

	Mixed Migrant: NSD* = delta/(1+exp((theta-t)/phi))- (delta*zeta)/(1+exp((theta+2*phi+2*phi2+rho-t)/phi2))

	Migrant:       NSD* = delta/(1+exp((theta-t)/phi)) - delta/(1+exp((theta+2*phi+2*phi2+rho-t)/phi2))

	Disperser:     NSD* = delta/(1+exp((theta-t)/phi))

	Nomad:         NSD* = beta * t

	Resident:      NSD* = gamma*(1-exp(kappa*t)

where delta representd the distance between ranges, theta the midpoint (in time) of the departing movement, phi the duration between completing 1/2 and 3/4 of the movement, rho the duration of migratory-range occupancy, zeta is the proportion of the outgoing migratory distance that a mixed migrant covers during the return movement, t is time in julian days, gamma is the mean NSD of an individual's range and beta and kappa are constants. If \code{rloc} is set to a value other than one, these models will be fit to relative NSD (rNSD) with \code{rloc} indicating the record number of the reference location from wich rNSD is calculated.

The structure and interpretation of "elev" models differ even though their structure is similar. For typ = "elev", three models are fit:

	Migrant:   elevation = gamma+delta/(1+exp((theta-t)/phi))-delta/(1+exp((theta+2*phi+2*phi2+rho-t)/phi2))

	Disperser: elevation = gamma+delta/(1+exp((theta-t)/phi))

	Resident:  elevation = gamma

where gamma represents the mean elevation (rather than mean NSD) on the starting range and all other parameters are itnerpreted as above.

The \code{stdt} argument standardize the calendar day from which movement events (i.e. theta values) are estimated. This argument also affects how \code{mvmts} objects are plotted, serving to standardize the starting point for the x-axis. (In some cases the choice of \code{stdt} will mean that some locations used in model fitting are not visible on \code{mvmt} plots.) The \code{scut} and \code{ecut} arguments require the same input as \code{stdt} and allow you to exclude points at the start or end of each trajectory from inclusion in fitting the model. Points before \code{scut} or after \code{ecut} will be excluded from model fitting, but will still appear in plots of \code{mvmt} objects unless a \code{stdt} is set to a value immediately following these locaitons.
}
\value{
Returns object of class \code{mvmts} containing model results}

\references{
Bunnefeld, N., L. Borger, B. van Moorter, C. M. Rolandsen, H. Dettki, E. J. Solberg, and G. Ericsson. (2011) A model-driven approach to quantify migration patterns: individual, regional and yearly differences. Journal of Animal Ecology 80, 466-476.

Spitz, D. (2015). Does Migration Matter? Causes and Consequences of Migratory Behavior in Sierra Nevada Bighorn Sheep. Dissertation thesis, University of Montana.
}
\author{Derek Spitz}

\seealso{
\code{\link{pEst}}
\code{\link{mvmt2df}}
\code{\link{refine}}
\code{\link{theta2}}
\code{\link{topmvmt}}
}
\examples{
data(bighorn)

pest.e <- pEst(s.d = -600)
bhs.elev <- mvmtClass(bighorn, fam = "elev", p.est = pest.e)
bhs.elev
plot(bhs.elev[[2]])
}
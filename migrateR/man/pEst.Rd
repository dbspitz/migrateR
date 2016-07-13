\name{pEst}
\alias{pEst}
\title{Starting Parameter Estimates and Constraints }
\description{ This function creates a matrix of starting parameter estimates and constraints as required by \code{mvmtClass} for fitting non-linear animal-movement models.}
\usage{
pEst(l.g = NA,  l.t = 1,   l.p = 1,  l.d = NA, l.r = 1,   l.p2 = 1,  l.z = NA, l.k = NA,
     s.g = NA, 	s.t = 90,  s.p = 7,  s.d = NA, s.r = 90,  s.p2 = 7,	 s.z = NA, s.k = NA,
	 u.g = NA,	u.t = 364, u.p = 21, u.d = NA, u.r = 364, u.p2 = 21, u.z = NA, u.k = NA)
}
\arguments{
  \item{l.g}{ lower gamma value}
  \item{l.t}{lower theta value}
  \item{l.p}{lower phi value}
  \item{l.d}{lower delta value}
  \item{l.r}{lower rho value}
  \item{l.p2}{lower phi2 value}
  \item{l.z}{lower zeta value}
  \item{l.k}{lower kappa value}
  \item{s.g}{starting gamma value}
  \item{s.t}{starting theta value}
  \item{s.p}{starting phi value}
  \item{s.d}{starting delta value}
  \item{s.r}{starting rho value}
  \item{s.p2}{starting phi2 value}
  \item{s.z}{starting zeta value}
  \item{s.k}{starting kappa value}
  \item{u.g}{upper gamma value}
  \item{u.t}{upper theta value}
  \item{u.p}{upper phi value}
  \item{u.d}{upper delta value}
  \item{u.r}{upper ho value}
  \item{u.p2}{upper phi2 value}
  \item{u.z}{upper zeta value}
  \item{u.k}{upper kappa value}
}


\details{
This funcion provides a means to supply the starting parameter values and constraints required by the `mvmtClass` function (see `mvmtClass` for a definition of terms). Default values are supplied for all required fields; \code{\link{mvmtClass}} will dynamically fill any remaining \code{NA} fields dynamically. Default inputs will not be appropriate for all trajectories and will require alteration before some trajectories can be successfully fit.
}
\value{
Returns a matrix formatted to match the requirements for the \code{p.est} argument of \code{\link{mvmtClass}}
}
\author{Derek Spitz}
\seealso{
\code{\link{mvmtClass}}
}
\examples{
  data(bighorn)

  # default constraints: triggers warning, one models not fit
  mvmtClass(bighorn,fam="elev",stdt="10-31")
  

  # revised starting delta value: all models fit, no warnings triggered
  p.est <- pEst(s.d=-500)
  mvmtClass(bighorn,fam="elev",stdt="10-31",p.est=p.est)

}

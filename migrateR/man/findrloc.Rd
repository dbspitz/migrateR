\name{findrloc}
\alias{findrloc}
\title{Find Best Supported Reference Location}
\description{ This function identifies the reference location that minimizes AIC within a family of NSD models. }
\usage{
findrloc(ltraj, max.rloc = 15, verbose = F, graph = F ...)
}
\arguments{
  \item{ltraj}{ object of class \code{\link{ltraj}}} 
  \item{max.rloc}{either the number of days from first location to consider as possible reference dates (numeric) or the maximum day to consider (character, formatted as "\%m-\%d")}
  \item{verbose}{logical: print AIC value and top model for all reference locations?}
  \item{graph}{logical: provide plot of AIC value by reference location? }
  \item{...}{additional arguments as described in \code{\link{mvmtClass}}}

}
\details{
This function will calculate relative net squared displacement (rNSD) from each reference location up to \code{max.rloc}, fit movement models to NSD and to each set of rNSD values using the function \code{\link{mvmtClass}}, and identify the reference location that results in a top model with the lowest overal AIC. 
}

\value{
Returns a \code{\link{data.frame}} with one record for each burst in \code{ltraj} and including columns for burst ("burst"), the reference date's timestamp ("location"), the reference location's record number ("rloc") and the best supported model fit to rNSD calculated from the reference location ("model"; determined by a call to \code{\link{topmvmt}}).
}
\author{ Derek Spitz }

\seealso{
\code{\link{mvmtClass}}
\code{\link{topmvmt}}
}
\examples{

# Select Single Trajectory (to reduce runtime)
data(elk)
YL15 <- elk[id(elk)=="YL15"]

# Set startdate and starting parameter estimates/constraints
stdt <-  "3-1"
p.est <- pEst(s.d=100)
max.rloc <- "3-10"

# Fit rNSD Model
YL15.rloc <- findrloc(YL15, stdt=stdt,p.est=p.est,verbose=T)
YL15.rnsd <- mvmtClass(YL15,rloc=YL15.rloc$rloc, stdt=stdt,p.est=p.est)


# compare to NSD Results
YL15.nsd <- mvmtClass(YL15,stdt=stdt,p.est=p.est)

  # AIC
  rnsd.minAIC <- min(sapply(YL15.rnsd[[1]]@models,AIC))
  nsd.minAIC <- min(sapply(YL15.nsd[[1]]@models,AIC))
  
  rnsd.minAIC - nsd.minAIC

  # Plots
  par(mfrow=1:2)
  plot(YL15.nsd[[1]],ylim=c(0,300))
  plot(YL15.rnsd[[1]],ylim=c(0,300))
}

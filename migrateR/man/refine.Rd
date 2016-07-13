\name{refine}
\alias{refine}
\title{Refine Movement Model Fit}
\description{ This function tests for improved fit to movement models, using new parameter constraints and/or starting values}
\usage{
refine(mvmt, p.est)
}
\arguments{
  \item{mvmt}{ object of class \code{mvmts} or \code{mvmt}, as from the output of \code{\link{mvmtClass}}}
  \item{p.est}{ starting paramter values and constraints, as from \code{\link{pEst}} }
}
\details{
This function attempts to fit models to \code{z} bursts from \code{mvmts} using the starting paramter evalues and constraints supplied by \code{p.est}, but otherwise retaining the argumetns used in the call that created \code{mvmts}. If the resulting models were missing from \code{mvmts}, they are added, otherwise they are compared to the equivalent model form \code{mvmts} and the model with lower AIC is retained.
}
\value{
Returns an object of class `mvmts` or `mvmt` with length equal to the number of bursts inclued in . 
}
\author{ Derek Spitz }

\seealso{
\code{\link{fullmvmt}}
\code{\link{mvmtClass}}
\code{\link{pEst}}
}
\examples{
  data(bighorn)

  # Starting parameter estimates/constraints
  pest.n <- pEst(s.d = 5)
  pest.n2 <- pEst(s.d = 10)

  # using either set these sets of values leaves missing models
    # using "pest.n" triggers warnings, leaves 3 models missing
    test1 <- mvmtClass(bighorn, p.est = pest.n, stdt = "10-31")
      fullmvmt(test1)

    # using "pest.n2" triggers warnings, leaves 3 models missing
    test2 <- mvmtClass(bighorn, p.est = pest.n2, stdt = "10-31")
      fullmvmt(test2)  # Note that the missing models differ between bhs.nsd and bhs.nsd2

  # refining one result wtih the other parameter estimates returns a complete model set
  bhs.nsd2 <- refine(test, pest.n2) # or: "refine(test2, pest.n)"
    fullmvmt(bhs.nsd2)
}
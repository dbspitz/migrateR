\name{topmvmt}
\alias{topmvmt}
\title{  Identify Top Movement Model }
\description{ This funciton identifies the best supported movement model from an object of class \code{mvmt} or for each burst from an object of class \code{mvmts}.}
\usage{
topmvmt(mvmt, omit = NA, a.rule=T, mdelta = NA, mrho = NA)
}
\arguments{
  \item{mvmt}{ object of class \code{mvmt} or \code{mvmts}, as from the output of \code{\link{mvmtClass}}}
  \item{omit}{ character: optional vector naming models to be excluded from consideration}
  \item{a.rule}{ logical; observe Arnold's Rule?}
  \item{mdelta}{ numeric: optional minimum threshold for "delta" estimate.}
  \item{mrho}{ numeric: optional minimum threshold for "rho" estimate.}  
}
\details{
Omit may only take values matching the model names found in the \code{mvmt}(s) object (\code{names(mvmt@models)}) 
If the \code{mdelta} argument is included, any models containing an estimate of delta with an absolute value below \code{mdelta} are excluded from consideration. Similarly, if the \code{mrho} arguments is included, any models containing an estimate of rho less than the value chosen are excluded from consideration.
}
\value{
Returns a named list of models, with one element for each burst included in \code{mvmt}.
}
\references{
Arnold, T. W. 2010. Uninformative parameters and model selection using Akaike’s Information Criterion. Journal of Wildlife Management 74:1175–1178.
}
\author{ Derek Spitz }

\seealso{
\code{\link{mvmtClass}}
}
\examples{
  data(bighorn)

  elev <- mvmtClass(bighorn, fam = "elev", stdt = "10-31")
  elev2 <- refine(elev, pEst(s.t = 15))
    fullmvmt(elev2)
 
  # classificaiton 1: default
  class1 <- topmvmt(elev)
  classified1 <- names(class1) # vector of classifications
  table(classified1)           # table of default classificaitons
  mvmt2df(class1)                # dataframe of parameter estimates
                               # **note that for the first migrant rho == 1

  # classification 2: constraint on rho
  class2 <- topmvmt(elev, mrho = 21)
  classified2 <- names(class2)
  table(names(class2))         # revised classification table (2)

  # closer look at change in classification:
  mvmt2df(class2) # parameter estimates for changed model(s)
                               # **note that delta is comparatively small (~200m)

  # classificaiton 3: constraints on lambda and delta
  class3 <- topmvmt(elev, mrho = 21, mdelta = 500)
  table(names(class3))         # revised classification table (3)
}
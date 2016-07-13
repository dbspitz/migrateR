\name{mvmt2df}
\alias{mvmt2df}
\title{ Extract Movement Model Parameter Estimates }
\description{ This function returns parameter estimates from non-linear animal movement, grouped into data.frames by model type.}
\usage{
mvmt2df(tmvmt)
}
\arguments{
  \item{tmvmt}{ named list of non-linear animal-movement models, as from the output of \code{\link{topmvmt}}}
}  
\details{
Elements of input must be named and of class "nls", "glm" and/or "lm". For a further discussion of these models and their parameterization, see \code{\link{mvmtClass}} and Bunnefeld et al. 2011.
}
\value{
Outputs a named list including a data.frame of paramter estimates for each observed model type
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

  elk.tmvmt <- topmvmt(elk.nsd)

  mvmt2df(elk.tmvmt)

  # to extract parameter estimates for all models fit to a single burst
  mvmt2df(elk.nsd[[1]]@models)
  
}

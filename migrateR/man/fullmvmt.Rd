\name{fullmvmt}
\alias{fullmvmt}
\title{Check Movement Models' Fit}
\description{This function identify which/whether \code{mvmt} models were successfully fit.}
\usage{
fullmvmt(mvmt, out = "logic")
}
\arguments{
  \item{mvmt}{ object of class \code{mvmt} or \code{mvmts}, as from the output of \code{\link{mvmtClass}}}
  \item{out}{ character: the type of output desired, either "logic","numer" or "name"}
}
\details{
Checks each burst for movement models.
}
\value{
Output depends on the value of \code{out}. For "logic" and "numer" the function returns a vector, named by busrt indicating whether all models are present or how many models are present, respectively. For "name" the function returns a named list of character vectors of the models fit to each burst.
}
\author{ Derek Spitz }
\seealso{
\code{\link{mvmtClass}}
}
\examples{
data(bighorn)
bhs.nsd <- mvmtClass(bighorn)

fullmvmt(bhs.nsd)
fullmvmt(bhs.nsd, out = "numer")
fullmvmt(bhs.nsd, out = "name")
}

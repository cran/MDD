\name{gui.mdd}
\alias{gui.mdd}
\title{GUI for MDD Calculations}
\description{Allows for interactive entry of parameters for various minimum detectable 
difference programs.}
\usage{
gui.mdd()
}
\details{GUI prompts for parameters, then calls one of \code{\link{mdd.chisq}}, 
\code{\link{mdd.fisher.exact}}, \code{\link{mdd.mantelhaen}}, \code{\link{mdd.mantelhaen.pow}}, 
\code{\link{mdd.satterthwaite}}, or \code{\link{mdd.t.test}}.}
\value{The same value as whichever function is called.}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\note{Does some basic data validation before calling the other programs.}
\seealso{\code{\link{mdd.chisq}}, \code{\link{mdd.fisher.exact}}, \code{\link{mdd.mantelhaen}}, 
\code{\link{mdd.mantelhaen.pow}}, \code{\link{mdd.satterthwaite}}, \code{\link{mdd.t.test}}.}
\keyword{htest}

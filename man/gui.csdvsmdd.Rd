\name{gui.csdvsmdd}
\alias{gui.csdvsmdd}
\title{GUI for Comparing CSD and MDD}
\description{Allows for interactive entry of parameters for various programs to compare
the clinically significant difference and the minimum detectable difference.}
\usage{
gui.csdvsmdd()
}
\details{GUI prompts for parameters, then calls one of \code{\link{csdvsmdd.chisq}}, 
\code{\link{csdvsmdd.fisher}}, \code{\link{csdvsmdd.satter}}, or \code{\link{csdvsmdd.t}}.}
\value{
  A list with the following three components:
  \item{statsig.not.clinsig}{the probability that the trial will have a statistically 
  significant but clinically insignificant result.}
  \item{clinsig.not.statsig}{the probability that the trial will have a clinically 
  significant but statistically insignificant result.}
  \item{power}{the power of the test.}
}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\note{Does some basic data validation before calling the other programs.}
\seealso{\code{\link{csdvsmdd.chisq}}, \code{\link{csdvsmdd.fisher}}, 
\code{\link{csdvsmdd.satter}}, \code{\link{csdvsmdd.t}}.}
\keyword{htest}

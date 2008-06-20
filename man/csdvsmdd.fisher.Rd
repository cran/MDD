\name{csdvsmdd.fisher}
\alias{csdvsmdd.fisher}
\title{CSD vs. MDD for Fisher's Exact Test}
\description{
For a trial evaluated using Fisher's exact test, calculates the probabilities of either 
having a clinically significant difference declared statistically insignificant, or of 
having a statistically significant difference declared clinically insignificant.  Also 
ouputs the power of the test.
}
\usage{
csdvsmdd.fisher(n.placebo, n.treat, prob.placebo, prob.treat, csd, alpha = 0.05, 
                alternative = "two.sided")
}
\arguments{
  \item{n.placebo}{number of subjects in control group.}
  \item{n.treat}{number of subjects in treatment group.}
  \item{prob.placebo}{anticipated proportion of responses in control group.}
  \item{prob.treat}{anticipated proportion of responses in treatment group.}
  \item{csd}{clinically significant difference.}
  \item{alpha}{significance level.}
  \item{alternative}{indicates the alternative hypothesis and must be one of \code{"two.sided"}, 
\code{"greater"}, or \code{"less"}.  You can specify just the initial letter.}
}
\details{
The \dQuote{clinically significant difference} is the smallest difference that will be
meaningful either clinically or commercially.  This is usually obtained from the clinical 
scientists and/or business partners you are working with.
}
\value{
  A list with the following three components:
  \item{statsig.not.clinsig}{the probability that the trial will have a statistically 
  significant but clinically insignificant result.}
  \item{clinsig.not.statsig}{the probability that the trial will have a clinically 
  significant but statistically insignificant result.}
  \item{power}{the power of the test.}
}
\references{See \code{\link{fisher.test}} for references.}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\seealso{\code{\link{csdvsmdd.chisq}} for an asymptotic test
 
\code{\link{gui.csdvsmdd}} for a GUI version}
\examples{
csdvsmdd.fisher(36, 36, .25, .6, .25)

#Note that both possibilities are represented here.

csdvsmdd.fisher(20, 20, .25, .6, .25)
csdvsmdd.fisher(20, 20, .25, .6, .45)

#Compare to the same commands using csdvsmdd.chisq; the first leads to identical 
#results, while the second and third give different results.
\dontshow{}
}
\keyword{htest}

\name{csdvsmdd.t}
\alias{csdvsmdd.t}
\title{CSD vs. MDD for t-Test (Equal Variances)}
\description{
For a trial evaluated using a t-test with equal variances, calculates the probabilities 
of either having a clinically significant difference declared statistically insignificant, 
or of having a statistically significant difference declared clinically insignificant.  
Also ouputs the power of the test.
}
\usage{
csdvsmdd.t(n1, n2, sigma, ate, csd, alpha = 0.05, alternative = "two.sided")
}
\arguments{
  \item{n1}{number of subjects in control group.}
  \item{n2}{number of subjects in treatment group.}
  \item{sigma}{anticipated common standard deviation.}
  \item{ate}{anticipated treatment effect.}
  \item{csd}{clinically significant difference.}
  \item{alpha}{significance level.}
  \item{alternative}{indicates the alternative hypothesis and must be one of \code{"two.sided"}, 
\code{"greater"}, or \code{"less"}.  You can specify just the initial letter.}
}
\details{
The \dQuote{clinically significant difference} is the smallest difference that will be
meaningful either clinically or commercially.  This is usually obtained from the clinical 
scientists and/or business partners you are working with.

The \dQuote{anticipated treatment effect} is the quantity on which the study will be powered;
it may, in general, be different from the clinically significant difference.
}
\value{
  A list with the following three components:
  \item{statsig.not.clinsig}{the probability that the trial will have a statistically 
  significant but clinically insignificant result.}
  \item{clinsig.not.statsig}{the probability that the trial will have a clinically 
  significant but statistically insignificant result.}
  \item{power}{the power of the test.}
}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\seealso{\code{\link{csdvsmdd.satter}} for unequal variances
 
\code{\link{gui.csdvsmdd}} for a GUI version}
\examples{
#Note that, unlike for csdvsmdd.chisq and csdvsmdd.fisher,
#at most one of the first two components can be zero.

csdvsmdd.t(20, 20, .35, .4, .3)
csdvsmdd.t(20, 20, .35, .4, .1)
}
\keyword{htest}

\name{mdd.t.test}
\alias{mdd.t.test}
\title{MDD for Student's t-Test (Equal Variances)}
\description{
Calculates minimum detectable difference for a two-sample t-test with equal variances.}
\usage{
mdd.t.test(n1, n2, mu1, sigma, alpha = 0.05, alternative = "two.sided", 
           return.cis = FALSE)
}
\arguments{
  \item{n1,n2}{number of subjects in each sample.}
  \item{mu1}{anticipated mean in sample 1 (control).}
  \item{sigma}{assumed common standard deviation.}
  \item{alpha}{significance level.}
  \item{alternative}{indicates the alternative hypothesis and must be one of \code{"two.sided"}, 
\code{"greater"} or \code{"less"}.  You can specify just the initial letter.}
  \item{return.cis}{a logical which indicates whether to return endpoints of confidence 
intervals or just the MDD.}
}
\details{
\code{alternative = "less"} is the alternative that has the treatment group mean 
less than the control group mean.
}
\value{
If \code{return.cis = FALSE}, the returned value is the MDD.

If \code{return.cis = TRUE}, the returned value is a numeric vector of length two 
giving the closest values to \code{mu1} that would be significant given the remaining 
parameters.
}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\note{For unequal variances, use \code{\link{mdd.satterthwaite}}}
\seealso{\code{\link{gui.mdd}} for a GUI version}
\examples{
mdd.t.test(20, 15, 3.1, .5)
mdd.t.test(20, 15, 3.1, .5, return.cis=TRUE)
}
\keyword{htest}

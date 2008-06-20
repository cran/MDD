\name{mdd.fisher.exact}
\alias{mdd.fisher.exact}
\title{MDD for Fisher's Exact Test}
\description{
Calculate the minimum detectable difference for a two-sample Fisher's exact test.
Also computes the true treatment response rates closest to the control response 
rate that would be detected with a given power.}
\usage{
mdd.fisher.exact(placebo.size, treat.size, prob.placebo, alpha = 0.05, pow = 0.8, 
                 alternative = "two.sided")
}
\arguments{
  \item{placebo.size}{number of subjects in control group.}
  \item{treat.size}{number of subjects in treatment group.}
  \item{prob.placebo}{anticipated proportion of responses in control group.}
  \item{alpha}{significance level.}
  \item{pow}{power.}
  \item{alternative}{indicates the alternative hypothesis and must be one of \code{"two.sided"}, 
\code{"greater"}, or \code{"less"}.  You can specify just the initial letter.}
}
\details{
\code{alternative = "less"} is the alternative that has the treatment group mean 
less than the control group mean.
}
\value{
 A list with the following components:
  \item{actual}{true treatment reponse rate(s) that would be detected as significant, 
  with power \code{pow}, at significance level \code{alpha}.}
  \item{observed}{matrix with the number and proportion of observed treatment 
  responses that would be significant, conditional on the number and proportion 
  of observed control responses.}
}
\references{See \code{\link{fisher.test}} for references.}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\seealso{\code{\link{chisq.test}} for an asymptotic test
 
\code{\link{gui.mdd}} for a GUI version}
\examples{
fish.ex <- mdd.fisher.exact(20,40,.25)

#For a two-sided test at significance level 0.05 and power 80\%, with 20 control 
#subjects and 40 treatment subjects and an assumed control response rate of 25\%.

fish.ex$actual

#If the true treatment response rate is less than 0.01811086 or greater than
#0.65370397, it will be detected with 80\% power.

fish.ex$observed

#For example, if there are 6 observed control repsonses (observed response rate 
#of 30\%), then either 3 or fewer or 25 or more treatment responses (7.5\% and 
#62.5\% response rates, respectively) will be declared significant.
#
#Compare to mdd.chisq(20,40,.25).

rm(fish.ex)
}
\keyword{htest}

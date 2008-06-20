\name{mdd.chisq}
\alias{mdd.chisq}
\title{MDD for Pearson's Chi-squared Test}
\description{
Finds the minimum detectable difference for a two-group, two-outcome experiment using a 
Pearson's chi-squared test statistic with continuity correction.  Also computes the true
treatment response rates closest to the control response rate that would be detected with 
a given power.
}
\usage{
mdd.chisq(placebo.size, treat.size, prob.placebo, alpha = 0.05, pow = 0.8)
}
\arguments{
  \item{placebo.size}{number of subjects in control group.}
  \item{treat.size}{number of subjects in treatment group.}
  \item{prob.placebo}{anticipated proportion of responses in control group.}
  \item{alpha}{significance level.}
  \item{pow}{power.}
}
\value{
 A list with the following components:
  \item{actual}{true treatment reponse rate(s) that would be detected as significant, 
  with power \code{pow}, at significance level \code{alpha}.}
  \item{observed}{matrix with the number and proportion of observed treatment 
  responses that would be significant, conditional on the number and proportion 
  of observed control responses.}
}
\references{See \code{\link{chisq.test}} for references.}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\seealso{\code{\link{mdd.fisher.exact}} for an exact (conditional) test
 
\code{\link{gui.mdd}} for a GUI version}
\examples{
chisq <- mdd.chisq(20,40,.25)

#For a test at significance level 0.05 and power 80\%, with 20 control 
#subjects and 40 treatment subjects and an assumed control response rate of 25\%.

chisq$actual

#If the true treatment response rate is greater than 0.6635562, it will be 
#detected with 80\% power.

chisq$observed

#For example, if there are 6 observed control repsonses (observed response rate 
#of 30\%), then either 2 or fewer or 25 or more treatment responses (5\% and 
#62.5\% response rates, respectively) will be declared significant.
#
#Compare to mdd.fisher.exact(20,40,.25).

rm(chisq)
}
\keyword{htest}

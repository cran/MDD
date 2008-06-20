\name{mdd.mantelhaen.pow}
\alias{mdd.mantelhaen.pow}
\title{Power Calculations for Cochran-Mantel-Haenszel Test}
\description{
Computes the true treatment response rates in each stratum closest to the corresponding 
stratum's control response rate that would be detected with a given power for a two-sample 
Cochran-Mantel-Haenszel (stratified chi-squared) test with arbitrary number of strata.  
Optionally, saves data to a file that later can be used to calculate the minimum detectable 
difference.
}
\usage{
mdd.mantelhaen.pow(placebo.size, treat.size, prob.placebo, alpha = 0.05, pow = 0.8, 
               alternative = "two.sided", exact = TRUE, which.strata = "all", 
               size.matrix, to.file = "", from.file = "")
}
\arguments{
  \item{placebo.size}{a vector with the number of subjects in each stratum assigned to the 
  control group; if omitted, will be derived from \code{size.matrix}.}
  \item{treat.size}{a vector with the number of subjects in each stratum assigned to the 
  treatment group; if omitted, will be derived from \code{size.matrix}.}
  \item{prob.placebo}{a vector of anticipated proportion of responses in each stratum for 
  the control group (but see Details, below).}
  \item{alpha}{significance level.}
  \item{pow}{power.}
  \item{alternative}{indicates the alternative hypothesis and must be one of \code{"two.sided"}, 
\code{"greater"}, or \code{"less"}.  You can specify just the initial letter.}
  \item{exact}{a logical value indicating whether to do an exact conditional test or use 
  the C-M-H test statistic.}
  \item{which.strata}{a list of vectors, each of which is a subset of \code{1:length(placebo.size)}.
These will be the stratum/strata in which the treatment mean will be allowed to differ from the 
control mean.}
  \item{size.matrix}{a matrix with two columns, the first of which becomes \code{placebo.size} 
  and the second of which becomes \code{treat.size}.}
  \item{to.file}{the path for the file in which to save the array of responses that are significant 
  for further analysis.}
  \item{from.file}{the path for the file containing a response array saved with a previous 
  \code{mdd.mantelhaen.pow} command with \code{to.file} given.  \code{placebo.size} and 
  \code{treat.size} are deduced from the loaded file.}
}
\details{
\code{alternative = "less"} is the alternative that has the treatment group mean 
less than the control group mean.

If present, the value of \code{size.matrix} will overwrite both \code{placebo.size} 
and \code{treat.size}.

\code{prob.placebo} can be specified as a single number, in which case it is assumed that
the response rate in the control group is constant across the strata.  In fact, currently the 
calculations will use \code{prob.placebo[1]} as the response rate in all control strata.  
This may change in a future version of the package.

The default value of \code{which.strata = "all"} translates to all 
\eqn{2^{\texttt{length(placebo.size)}} - 1}{\code{2^length(placebo.size) - 1}}
possible non-empty subsets of strata.

The array that is stored in \code{to.file} is called \code{sig.inds} and has the treatment 
strata numbers in odd dimensions and the control strata numbers in even dimensions.  
Specifically, if \code{placebo.size = c(2,3,4)} and \code{treat.size = c(5,6,7)}, then 
\code{dim(sig.inds) = c(6,3,7,4,8,5)}.  (Note that the dimensions are one more than the
corresponding sizes because the number of responses in each stratum can be \code{0} or any 
number up to the size of the stratum.)  The entries of \code{sig.inds} are \code{1} for a 
significant test, \code{0} otherwise.  In particular, \code{sig.inds[c(1,1,1,1,1,1)]} would 
indicate whether or not the test is significant if there were no responses, and 
\code{sig.inds[c(4,2,6,1,5,3)]} would indicate the significance of the test with 
\code{c(3,5,4)} treatment responses and \code{c(1,0,2)} control responses.

If \code{to.file} does not already end in \dQuote{\code{.RData}}, that suffix is appended to 
\code{to.file}.
}
\value{
The value is a matrix of dimension \code{c(length(which.strata),2)} (if 
\code{alternative = "two.sided"}) or \code{c(length(which.strata),1)}, giving the closest 
true treatment response rate(s) to \code{prob.placebo[1]} that would be detected as significant 
with power \code{pow} at significance level \code{alpha} if the strata corresponding to 
\code{which.strata} differed from the control.  (See the Examples section for an illustration.)
}
\references{See \code{\link{mantelhaen.test}} for references.}
\seealso{\code{\link{extract.mdd.mantelhaen}}, \code{\link{mdd.mantelhaen}}

\code{\link{gui.mdd}} for a GUI version}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\section{Warning}{
Can have very long run times when there are three or more strata, or when there are a large 
number of subjects in all strata combined.

The program that does the actual calculations to determine the significant combinations of
treatment and control responses in each stratum, \code{.mantelhaen}, is flawed, because it 
assumes that if a test is significant with fewer control responses than treatment responses, then
either changing a treatment non-responder to a responder or changing a control responder to a 
non-responder will maintain significance.  This is not, however, true.  For example, with 
\code{placebo.size = c(3,4,5)} and \code{treat.size = c(8,7,6)}, the p-value for a two-sided test
with \code{c(0,1,0)} control responses and \code{c(0,0,6)} treatment responses is \code{0.04820937},
but increasing the treatment responses to \code{c(1,0,6)} changes the p-value to \code{0.05507853}.
In this particular example, this leads to the program incorrectly identifying 4 tests as not 
significant when they are actually significant.  Since there are actually 12,364 significant 
tests in this example out of a total of 151,200 tests (identified via exhaustive search), the 
program's result should be pretty close to the actual answer, and it seems unlikely that this 
phenomenon will be a common occurrence in the general case.  Nevertheless, the user should be 
aware of this illustration of Simpson's paradox.  In future versions of the package, this will 
hopefully be corrected.  (Currently, all the  solutions I have thought of will drastically 
increase the run time for a very small gain, so I will leave them for future implementation.)
}
\examples{
sm <- matrix(c(3,4,5,6,7,8), ncol=2)
wh <- list(1,c(1,2),c(1,3),c(1,2,3)) 
mdd.mantelhaen.pow(prob.placebo = 0.25, which.strata = wh, size.matrix = sm)

#Takes about 33 seconds in R 2.5.1 on a machine with a Pentium M 1.86GHz 
#processor and 1 GB RAM.
#
#If only strata 1 and/or 2 are different from the control, even true treatment 
#response rates of 0\% or 100\% in those strata will not be detected with 80\% 
#power.
#
#If both strata 1 and 3 have true treatment response rates of 0.9578682 or more, 
#it will be detected with 80\% power.
#
#If all three strata have true treatment response rates of 0.7717062 or more, 
#it will be detected with 80\% power.

rm(sm,wh)
}
\keyword{htest}


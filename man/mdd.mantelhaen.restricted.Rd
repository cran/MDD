\name{mdd.mantelhaen.restricted}
\alias{mdd.mantelhaen.restricted}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MDD for CMH Given Number of Observed Control Responses}
\description{
For a specified number of control responses in each strata, calculate all possible 
treatment response scenarios for which the test statistic will be signiifcant, and optionally 
(by default) print out a paragraph sumamrizing the results.
}
\usage{
mdd.mantelhaen.restricted(placebo.size, treat.size, placebo.vals, alpha = 0.05, alternative = "two.sided", exact = TRUE, print.summary = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{placebo.size}{a vector with the number of subjects in each stratum assigned to the control group.}
  \item{treat.size}{a vector with the number of subjects in each stratum assigned to the treatment group.}
  \item{placebo.vals}{the number of responses to be observed in the control strata.}
  \item{alpha}{significance level.}
  \item{alternative}{indicates the alternative hypothesis and must be one of <code>"two.sided"</code>, 
<code>"greater"</code> or <code>"less"</code>.  You can specify just the initial letter.}
  \item{exact}{a logical value indicating whether to do an exact conditional test or use the CMH test statistic.}
  \item{print.summary}{a logical indicating whether to print a paragraph summarizing the calculations.}
}
\value{
  \item{le}{All treatmenmt response scenarios for which the test statistic will be significant 
  by virtue of the treatment response rate being less than the control response rate.}
  \item{gr}{All treatmenmt response scenarios for which the test statistic will be significant 
  by virtue of the treatment response rate being greater than the control response rate.}
  ...
}
\references{See \code{\link{mantelhaen.test}} for references.}
\author{Don Barkauskas}
\note{
For one-sided tests, at least one of the two components of the return value will automatically be empty!

This function does the same job as \code{mdd.mantelhaen(..., to.file="file.Rdata")} followed 
by \code{extract.mdd.mantelhaen(..., from.file="file.RData")}, but much more quickly.
}
\section{Warning}{
Run times for this program can be extremely large for trials with either a large number of strata or a 
large number of subjects.
}
\seealso{\code{mdd.mantelhaen}, \code{extract.mdd.mantelhaen}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}% __ONLY ONE__ keyword per line

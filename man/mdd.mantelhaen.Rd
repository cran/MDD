\name{mdd.mantelhaen}
\alias{mdd.mantelhaen}
\title{MDD for Cochran-Mantel-Haenszel Test}
\description{
For a specified number of control responses in each stratum, calculate all possible 
treatment response scenarios for which the Cochran-Mantel-Hanszel test will be 
significant, and optionally (by default) print out a paragraph summarizing the results.
}
\usage{
mdd.mantelhaen(placebo.size, treat.size, placebo.vals, alpha = 0.05, 
               alternative = "two.sided", exact = TRUE, print.summary = TRUE)
}
\arguments{
  \item{placebo.size}{a vector with the number of subjects in each stratum assigned to the control group.}
  \item{treat.size}{a vector with the number of subjects in each stratum assigned to the treatment group.}
  \item{placebo.vals}{a vector with the number of responses to be observed in each control stratum.}
  \item{alpha}{significance level.}
  \item{alternative}{indicates the alternative hypothesis and must be one of \code{"two.sided"}, 
\code{"greater"}, or \code{"less"}.  You can specify just the initial letter.}
  \item{exact}{a logical value indicating whether to do an exact conditional test or use the C-M-H test statistic.}
  \item{print.summary}{a logical value indicating whether to print a paragraph summarizing the calculations.}
}
\value{
  A list with the following two components:
  \item{le}{A matrix with rows consisting of all non-comparable (see below) treatment response 
  scenarios for which the test statistic will be significant by virtue of the treatment response 
  rate being less than the control response rate.}
  \item{gr}{A matrix with rows consisting of all non-comparable (see below) treatment response 
  scenarios for which the test statistic will be significant by virtue of the treatment response 
  rate being greater than the control response rate.}
  If \code{print.summary = TRUE}, then this is returned invisibly.
}
\references{See \code{\link{mantelhaen.test}} for references.}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\note{
\dQuote{Non-comparable} refers to the fact that if, for example, \code{c(2,3)} is in the \code{le}
component, then \code{c(2,2)}, \code{c(1,3)}, \code{c(1,2)}, etc., would all be significant as well.
On the other hand, \code{c(3,2)} and \code{c(0,4)} might or might not be.  In particular, for any 
two rows in the \code{le} component, there will be at least one column which is strictly greater 
in the first row than the second, and at least one column which is strictly greater 
in the second row than the first; and similarly for the \code{gr} component.

For one-sided tests, at least one of the two components of the return value will automatically 
be empty.

This function does the same job as \code{mdd.mantelhaen.pow(\dots, to.file="file.Rdata")} followed 
by \code{extract.mdd.mantelhaen(\dots, from.file="file.RData")}, but much more quickly.
}
\section{Warning}{
Run times for this program can be extremely long for data sets with either a large number of 
strata or a large number of subjects.

The Warning section of \code{\link{mdd.mantelhaen.pow}} explains a problem with the algorithm
used in that program which might or might not also show up for this program.  (It's virtually 
certain that it can, but I have not constructed an example to verify this.)
}
\seealso{\code{\link{extract.mdd.mantelhaen}}, \code{\link{mdd.mantelhaen.pow}}

\code{\link{gui.mdd}} for a GUI version}
\examples{
mh.ex <- mdd.mantelhaen(c(11,10,10), c(11,10,10), c(4,3,3))

#For a two-sided test at significance level 0.05, with 31 control subjects 
#(split 11, 10, and 10 among the strata) and 31 treatment subjects (split 11, 
#10, and 10 among the strata), and 10 observed control responses (split 4, 3, 
#and 3 among the strata).
#
#With 19 or more, or 2 or fewer, treatment responses, the test will be 
#significant no matter how the responses are distributed across the strata.  
#With exactly 18 treatment responses, whether or not the test is significant 
#depends on how the responses are distributed across the strata.

head(mh.ex$gr)

#The first row tells us, for example, that c(0,9,10), c(0,10,10), and c(1,9,10)
#would all be significant.

head(mh.ex$le)

#The second row tells us, for example, that c(0,1,1), c(0,0,1), and c(0,1,0)
#would all be significant.

rm(mh.ex)
}

\keyword{htest}




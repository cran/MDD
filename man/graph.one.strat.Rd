\name{graph.one.strat}
\alias{graph.one.strat}
\title{Plot Significant Scenarios for Fisher's Exact Test}
\description{Plots combinations of observed number of control and treatment responses
that will be significant using Fisher's exact test.  Will either plot a grid of the significant
scenarios or a grid weighted by the probability of the scenarios occurring.  In the latter
case, the power of the test will also be displayed on the graph.
}
\usage{
graph.one.strat(res.mat, weighted = FALSE, prob.placebo = NULL, prob.treat = NULL,
                show.grid = FALSE)
}
\arguments{
  \item{res.mat}{either a list like the one returned by \code{\link{mdd.fisher.exact}}, or
a matrix in the form of the \code{observed} component of such a list.}
  \item{weighted}{a logical indicating whether to weight the plot by probability.}
  \item{prob.placebo}{the assumed response rate in the control group.}
  \item{prob.treat}{the assumed response rate in the treatment group.}
  \item{show.grid}{a logical indicating whether to show a graph-paper-like grid on the plot.}
}
\details{
The resulting plot has all of the significant combinations in blue and the 
non-significant ones in white.

If \code{weighted = TRUE}, then each significant result is colored with an area 
proportional to its probability; thus, the total blue area is the power of the test, 
which is also displayed on the graph.
}
\value{No return value; the graph is simply plotted.}
\references{see \code{\link{fisher.test}} for references}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\note{ 
The values of \code{prob.placebo} and \code{prob.treat} are irrelevant if \code{weighted = FALSE}.
}
\seealso{\code{\link{gui.graph.one.strat}} for a GUI version}
\examples{
fish.ex <- mdd.fisher.exact(20, 40, .25)
graph.one.strat(fish.ex)
x11()
graph.one.strat(fish.ex, weighted = TRUE, .25, .6, show.grid = TRUE)
rm(fish.ex)
}
\keyword{htest}

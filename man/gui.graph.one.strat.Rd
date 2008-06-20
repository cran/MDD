\name{gui.graph.one.strat}
\alias{gui.graph.one.strat}
\title{GUI for Power in Fisher's Exact Test}
\description{Allows for interactive entry of parameters for \code{\link{graph.one.strat}}.}
\usage{
gui.graph.one.strat()
}
\details{GUI prompts for parameters, then calls \code{\link{graph.one.strat}}.}
\value{The matrix from the \code{observed} component returned by the last call to 
\code{\link{mdd.fisher.exact}}, returned invisibly.}
\author{Don Barkauskas (\email{barkda@wald.ucdavis.edu})}
\note{
First calls \code{\link{mdd.fisher.exact}} to generate the \code{res.mat} argument of 
\code{\link{graph.one.strat}}.

Each time the \dQuote{Graph} button on the GUI is pushed, it calls 
\code{\link{mdd.fisher.exact}} (if any of the parameters needed for that function 
have changed since the last time \dQuote{Graph} was pushed), then calls 
\code{\link{graph.one.strat}}.

The \dQuote{New} button on the GUI opens a new graphics window and makes it the 
active device.

Does some basic data validation before calling \code{graph.one.strat}.}

\seealso{\code{\link{mdd.fisher.exact}}, \code{\link{graph.one.strat}}}
\keyword{htest}

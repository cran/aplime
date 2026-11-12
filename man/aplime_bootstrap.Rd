\name{aplime_bootstrap}
\alias{aplime_bootstrap}
\title{Bootstrap Intervals for Grouped MARS Coefficients}
\description{Fits the local MARS model on resampled neighborhood rows, averaging coefficients with the same variable, hinge direction, and interaction structure while ignoring knot values. A group absent from a refit remains missing rather than becoming zero.}
\usage{
aplime_bootstrap(explanation, n_boot = 100L, seed = NULL)
}
\arguments{
\item{explanation}{Output from \code{aplime_explain()} with \code{keep_neighborhood = TRUE}.}
\item{n_boot}{Number of bootstrap resamples (at least two).}
\item{seed}{Optional random seed.}
}
\value{A data frame of coefficient-group names, point estimates, and percentile confidence limits.}

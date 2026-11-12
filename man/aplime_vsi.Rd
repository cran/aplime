\name{aplime_vsi}
\alias{aplime_vsi}
\title{Variable Stability Index}
\description{Measures mean pairwise overlap of selected top-feature sets, normalized by the requested feature count.}
\usage{
aplime_vsi(records, n_features = NULL)
}
\arguments{
\item{records}{List of repeated explanations.}
\item{n_features}{Number of top features in each explanation; inferred from the first record if omitted.}
}
\value{VSI on a 0 to 100 scale, or \code{NA_real_} for fewer than two runs.}

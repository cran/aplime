\name{aplime_repeat}
\alias{aplime_repeat}
\title{Repeat Local Explanations and Evaluate Stability}
\description{Generates repeated weighted MARS explanations and computes normal CSI, optional percentile-bootstrap CSI, variable stability, and rank-position frequencies. Terms are matched ignoring knot values; within-group coefficients are averaged. The \code{knot_report} output retains original terms, thresholds, and coefficients; \code{different_knot_groups} lists only matched groups containing different original terms. A warning identifies such groups.}
\usage{
aplime_repeat(explainer, instance, degree = 1L, n_repeats = 100L,
  n_samples = 5000L, n_features = 6L, n_boot = 0L, seed = NULL)
}
\arguments{
\item{explainer}{An \code{aplime_explainer} object.}
\item{instance}{Observation to explain.}
\item{degree}{MARS interaction degree: 1 or 2.}
\item{n_repeats}{Number of explanation repetitions (at least two).}
\item{n_samples}{Number of perturbations per repetition.}
\item{n_features}{Number of displayed features and top coefficient groups.}
\item{n_boot}{Number of bootstrap resamples, or zero to skip bootstrap CSI.}
\item{seed}{Optional random seed.}
}
\value{A list with \code{runs}, \code{vsi_percent}, \code{csi_normal_percent}, \code{csi_bootstrap_percent}, \code{rank_frequency}, \code{knot_report}, and \code{different_knot_groups}.}
\examples{
## Lightweight example; increase repetitions for a stability study.
\donttest{
set.seed(1)
x <- data.frame(a = rnorm(60), b = rnorm(60), c = rnorm(60))
f <- function(z) 1 + pmax(0, z[, 1] - 0.2) + z[, 2] * z[, 3]
e <- aplime_explainer(x, f)
out <- aplime_repeat(e, as.numeric(x[1, ]), n_repeats = 3,
                     n_samples = 80, n_features = 2, n_boot = 3)
out$csi_normal_percent
out$different_knot_groups
}
}

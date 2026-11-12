\name{aplime_explain}
\alias{aplime_explain}
\title{Fit a Local Weighted MARS Explanation}
\description{Samples a Gaussian neighborhood, applies distance weights, and fits a weighted \code{earth} surrogate. The displayed variable contributions are prediction differences; CSI is calculated separately from grouped MARS coefficients.}
\usage{
aplime_explain(explainer, instance, degree = 1L, n_samples = 5000L,
  n_features = 6L, seed = NULL, keep_neighborhood = FALSE)
}
\arguments{
\item{explainer}{An \code{aplime_explainer} object.}
\item{instance}{One numeric observation in background column order.}
\item{degree}{Interaction degree: 1 for additive hinge terms, 2 for two-way interactions.}
\item{n_samples}{Number of Gaussian perturbations including the original observation.}
\item{n_features}{Number of displayed variables, ranked by absolute prediction-difference contribution. Defaults to up to 6, capped at the number of available features.}
\item{seed}{Optional random seed.}
\item{keep_neighborhood}{Whether to retain samples and weights for coefficient interval estimation.}
}
\value{An \code{aplime_explanation} object containing an \code{earth} fit, contribution table, selected feature indices, and weighted in-sample R-squared.}

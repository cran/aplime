\name{aplime_explainer}
\alias{aplime_explainer}
\title{Create an Adaptive Piecewise Explainer}
\description{Stores a reference distribution and a prediction interface for local perturbations.}
\usage{
aplime_explainer(background, predict_function, kernel_width = NULL)
}
\arguments{
\item{background}{Numeric data frame or matrix with finite values.}
\item{predict_function}{Function returning one finite prediction for each numeric matrix row.}
\item{kernel_width}{Positive Gaussian kernel width; default is 0.75 times the square root of the predictor count.}
}
\value{An object of class \code{aplime_explainer}.}
\examples{
set.seed(1)
x <- data.frame(a = rnorm(30), b = rnorm(30))
f <- function(z) 2 * z[, 1] + z[, 2]
explainer <- aplime_explainer(x, f)
}

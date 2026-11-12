\name{aplime_predict_xgboost}
\alias{aplime_predict_xgboost}
\title{Adapt XGBoost Predictions for AP-LIME}
\description{Creates a prediction function suitable for \code{aplime_explainer()}. The optional \code{xgboost} package must be installed.}
\usage{
aplime_predict_xgboost(model)
}
\arguments{
\item{model}{An XGBoost Booster object or a model file path recognized by \code{xgboost::xgb.load()}.}
}
\value{A function taking a numeric matrix and returning numeric predictions.}

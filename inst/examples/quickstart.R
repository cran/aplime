library(aplime)
set.seed(123)
background <- data.frame(age = rnorm(60, 50, 10),
                         bmi = rnorm(60, 26, 4),
                         sbp = rnorm(60, 125, 15))
predict_function <- function(x) {
  1 + 0.04 * pmax(x[, 1] - 50, 0) + 0.08 * pmax(x[, 2] - 25, 0)
}
explainer <- aplime_explainer(background, predict_function)
result <- aplime_explain(explainer, as.numeric(background[1, ]),
                         degree = 1, n_samples = 100, n_features = 3)
print(result)
# Repeated stability evaluation:
# stability <- aplime_repeat(explainer, as.numeric(background[1, ]),
#                           n_repeats = 3, n_samples = 100,
#                           n_features = 3, n_boot = 5)
# stability$csi_normal_percent
# stability$csi_bootstrap_percent
# stability$different_knot_groups

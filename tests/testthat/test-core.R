test_that("same-direction knots are merged by arithmetic mean", {
  terms <- c("h(v1-25)" = 0.12, "h(v1-28)" = 0.03,
             "h(25-v1)" = -0.04)
  groups <- aplime:::.group_coefficients(terms)
  expect_equal(unname(groups[["h(v1-KNOT)"]]), 0.075)
  expect_equal(unname(groups[["h(KNOT-v1)"]]), -0.04)
})

test_that("interaction structure and direction are retained", {
  expect_identical(aplime:::.term_key("h(v1-20)*h(v2-10)"),
                   aplime:::.term_key("h(v1-25)*h(v2-12)"))
  expect_false(identical(aplime:::.term_key("h(v1-20)"),
                         aplime:::.term_key("h(20-v1)")))
})

test_that("CSI reports pairwise overlap on supplied intervals", {
  record <- function(low, high) list(coefficient_ci = list(normal =
    data.frame(term = "h(v1-KNOT)", estimate = 0.5, lower = low, upper = high)))
  expect_equal(aplime_csi(list(record(0, 1), record(0.5, 1.5)), "normal"), 100)
  expect_equal(aplime_csi(list(record(0, 1), record(2, 3)), "normal"), 0)
})

test_that("end to end lightweight explanation fits earth", {
  skip_if_not_installed("earth")
  set.seed(42)
  bg <- data.frame(a = rnorm(50), b = rnorm(50), c = rnorm(50))
  pred <- function(z) 2 + pmax(z[, 1] - 0.3, 0) + 0.5 * z[, 2]
  e <- aplime_explainer(bg, pred)
  one <- aplime_explain(e, as.numeric(bg[1, ]), n_samples = 100,
                        n_features = 2, keep_neighborhood = TRUE)
  expect_s3_class(one, "aplime_explanation")
  expect_equal(nrow(one$table), 2)
  basis <- stats::model.matrix(one$model)
  expect_equal(ncol(basis), length(stats::coef(one$model)))
  expect_identical(gsub("\\s+", "", colnames(basis)),
                   gsub("\\s+", "", names(stats::coef(one$model))))
  ci <- aplime:::.coefficient_intervals(one, "normal")
  expect_true(is.data.frame(ci))
  report <- suppressWarnings(
    aplime_repeat(e, as.numeric(bg[1, ]), n_repeats = 2,
                  n_samples = 100, n_features = 2, n_boot = 0)
  )
  expect_true(is.list(report))
  expect_true("different_knot_groups" %in% names(report))
})

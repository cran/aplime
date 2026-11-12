#' Construct an Adaptive Piecewise LIME Explainer
#'
#' @param background Numeric data frame or matrix used to generate Gaussian perturbations.
#' @param predict_function Function accepting a numeric matrix and returning one prediction per row.
#' @param kernel_width Positive kernel width. Defaults to 0.75 times square root of feature count.
#' @return An `aplime_explainer` object.
#' @export
aplime_explainer <- function(background, predict_function, kernel_width = NULL) {
  x <- .as_numeric_matrix(background, "background")
  if (nrow(x) < 2L) stop("background must have at least two rows")
  if (!is.function(predict_function)) stop("predict_function must be a function")
  p <- ncol(x)
  if (is.null(kernel_width)) kernel_width <- 0.75 * sqrt(p)
  if (length(kernel_width) != 1L || !is.finite(kernel_width) || kernel_width <= 0)
    stop("kernel_width must be positive")
  means <- colMeans(x)
  sds <- apply(x, 2L, stats::sd)
  sds[!is.finite(sds) | sds == 0] <- 1
  structure(list(background = x, predict_function = predict_function,
                 feature_names = colnames(x), means = means,
                 sds = sds, kernel_width = kernel_width), class = "aplime_explainer")
}

.as_numeric_matrix <- function(x, label) {
  if (!is.matrix(x) && !is.data.frame(x)) stop(label, " must be a data frame or matrix")
  if (is.data.frame(x) && !all(vapply(x, is.numeric, logical(1))))
    stop(label, " must contain only numeric features")
  if (is.matrix(x) && !is.numeric(x)) stop(label, " must be numeric")
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (!ncol(x) || !nrow(x) || any(!is.finite(x)))
    stop(label, " must be a nonempty finite numeric matrix")
  if (is.null(colnames(x))) colnames(x) <- paste0("feature", seq_len(ncol(x)))
  if (anyDuplicated(colnames(x))) stop(label, " must have unique column names")
  x
}

.instance <- function(explainer, instance) {
  if (!inherits(explainer, "aplime_explainer")) stop("Invalid explainer")
  if (is.data.frame(instance) || is.matrix(instance)) {
    if (nrow(instance) != 1L) stop("instance must contain one row")
    if (!is.null(colnames(instance)) &&
        !identical(colnames(instance), explainer$feature_names))
      stop("instance column names/order do not match background")
    instance <- as.numeric(instance[1L, ])
  }
  if (!is.numeric(instance) || length(instance) != length(explainer$means) ||
      any(!is.finite(instance))) stop("instance must contain one finite value per feature")
  instance
}

.neighborhood <- function(explainer, instance, n_samples) {
  p <- length(instance)
  z <- matrix(stats::rnorm(n_samples * p), nrow = n_samples)
  z <- sweep(sweep(z, 2L, explainer$sds, `*`), 2L, explainer$means, `+`)
  z[1L, ] <- instance
  colnames(z) <- explainer$feature_names
  standardized <- sweep(sweep(z, 2L, instance, `-`), 2L, explainer$sds, `/`)
  distances_sq <- rowSums(standardized^2)
  # Gaussian proximity weights.
  weights <- exp(-distances_sq / (2 * explainer$kernel_width^2))
  response <- as.numeric(explainer$predict_function(z))
  if (length(response) != n_samples || any(!is.finite(response)))
    stop("predict_function must return one finite prediction per neighborhood row")
  list(x = z, y = response, weights = weights)
}

.mars_fit <- function(x, y, weights, degree) {
  # Fit proximity-weighted MARS.
  d <- as.data.frame(x)
  names(d) <- paste0("v", seq_len(ncol(x)))
  d$target <- y
  earth::earth(target ~ ., data = d, weights = weights, degree = degree, trace = 0, keepxy = TRUE)
}

.mars_predict <- function(model, x) {
  d <- as.data.frame(x)
  names(d) <- paste0("v", seq_len(ncol(x)))
  as.numeric(stats::predict(model, newdata = d))
}

.contributions <- function(model, instance, reference) {
  p <- length(instance)
  modified <- matrix(rep(instance, times = p + 1L), nrow = p + 1L, byrow = TRUE)
  for (j in seq_len(p)) modified[j + 1L, j] <- reference[j]
  preds <- .mars_predict(model, modified)
  preds[1L] - preds[-1L]
}

#' Explain One Observation with AP-LIME or AP2-LIME
#'
#' @param explainer Object returned by [aplime_explainer()].
#' @param instance Numeric vector or one-row numeric matrix in background column order.
#' @param degree `1` for additive hinges; `2` for pairwise hinge interactions.
#' @param n_samples Number of Gaussian neighborhood observations.
#' @param n_features Number of reported top features (default: up to 6, capped at the number of features).
#' @param seed Optional random seed.
#' @param keep_neighborhood Retain perturbations for diagnostics and bootstrap.
#' @return An object of class `aplime_explanation` with MARS model, contribution table,
#'   fitted local fidelity and optional neighborhood.
#' @export
aplime_explain <- function(explainer, instance, degree = 1L, n_samples = 5000L,
                           n_features = 6L, seed = NULL, keep_neighborhood = FALSE) {
  instance <- .instance(explainer, instance)
  if (length(degree) != 1L || is.na(degree) || !degree %in% c(1, 2))
    stop("degree must be 1 or 2")
  if (length(n_samples) != 1L || is.na(n_samples) || n_samples < 10L || n_samples != as.integer(n_samples))
    stop("n_samples must be an integer of at least 10")
  p <- length(instance)
  if (missing(n_features)) n_features <- min(6L, p)
  if (length(n_features) != 1L || is.na(n_features) || n_features < 1L ||
      n_features > p || n_features != as.integer(n_features))
    stop("n_features must be an integer between 1 and the number of features")
  if (!is.null(seed)) set.seed(seed)
  neighborhood <- .neighborhood(explainer, instance, as.integer(n_samples))
  model <- .mars_fit(neighborhood$x, neighborhood$y, neighborhood$weights, degree)
  reference <- colSums(neighborhood$x * neighborhood$weights) / sum(neighborhood$weights)
  effects <- .contributions(model, instance, reference)
  order_ids <- order(-abs(effects))[seq_len(n_features)]
  table <- data.frame(feature = explainer$feature_names[order_ids],
                      contribution = effects[order_ids], rank = seq_along(order_ids))
  fitted <- .mars_predict(model, neighborhood$x)
  mean_y <- stats::weighted.mean(neighborhood$y, neighborhood$weights)
  sst <- sum(neighborhood$weights * (neighborhood$y - mean_y)^2)
  r_squared <- if (sst > 0) 1 - sum(neighborhood$weights * (neighborhood$y - fitted)^2) / sst else NA_real_
  structure(list(method = if (degree == 1L) "AP-LIME" else "AP2-LIME",
                 degree = degree, instance = instance, model = model,
                 contributions = stats::setNames(effects, explainer$feature_names),
                 top_features = order_ids, table = table,
                 reference = reference, weighted_r_squared = r_squared,
                 neighborhood = if (keep_neighborhood) neighborhood else NULL),
            class = "aplime_explanation")
}

#' @export
print.aplime_explanation <- function(x, ...) {
  cat(x$method, "explanation; weighted R-squared:",
      format(x$weighted_r_squared, digits = 3), "\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' Confidence Intervals for MARS Coefficient Groups
#'
#' Terms are matched by their variables, hinge directions, and interaction
#' structure, ignoring numeric knot locations. For example, h(v1-50) and
#' h(v1-52) have the same group key. h(50-v1) is a different group.
#' Multiple coefficients sharing a key within one model are averaged.
#' Normal CIs account for covariance between coefficients in the mean.
#' Bootstrap CIs use coefficient-group means from refitted models.
#' A missing bootstrap group is not treated as a zero coefficient.
#'
#' @param explanation An explanation created with keep_neighborhood = TRUE.
#' @param n_boot Number of bootstrap replications.
#' @param seed Optional seed.
#' @return A coefficient-group table with estimates and percentile limits.
#' @export
aplime_bootstrap <- function(explanation, n_boot = 100L, seed = NULL) {
  .coefficient_intervals(explanation, "bootstrap", n_boot, seed)
}

.coefficient_vector <- function(model) {
  b <- stats::coef(model)
  if (is.matrix(b)) b <- as.vector(b)
  if (is.null(names(b)) || any(!nzchar(names(b))))
    stop("MARS coefficient terms do not have identifiable names")
  b
}

# Match terms while ignoring knot values.
.term_key <- function(term) {
  term <- gsub("\\s+", "", term)
  number <- "[-+]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?"
  forward <- paste0("h\\((v[0-9]+)-(", number, ")\\)")
  backward <- paste0("h\\((", number, ")-(v[0-9]+)\\)")
  term <- gsub(forward, "h(\\1-KNOT)", term, perl = TRUE)
  term <- gsub(backward, "h(KNOT-\\2)", term, perl = TRUE)
  term
}

.term_knots <- function(term) {
  matches <- regmatches(term, gregexpr("h\\([^)]*\\)", term, perl = TRUE))[[1L]]
  if (!length(matches)) return("")
  paste(matches, collapse = " * ")
}

.group_coefficients <- function(coefs) {
  coefs <- coefs[names(coefs) != "(Intercept)" & is.finite(coefs)]
  keys <- vapply(names(coefs), .term_key, character(1))
  groups <- split(seq_along(coefs), keys)
  setNames(vapply(groups, function(idx) mean(coefs[idx]), numeric(1)), names(groups))
}

.coefficient_intervals <- function(explanation, ci_method = "normal",
                                   n_boot = 100L, seed = NULL) {
  if (!inherits(explanation, "aplime_explanation") || is.null(explanation$neighborhood))
    stop("The explanation must retain its neighborhood (keep_neighborhood = TRUE)")
  ci_method <- match.arg(ci_method, c("normal", "bootstrap"))
  if (!is.null(seed)) set.seed(seed)
  coefs <- .coefficient_vector(explanation$model)
  selected <- names(coefs) != "(Intercept)" & is.finite(coefs)
  terms <- names(coefs)[selected]
  empty <- data.frame(term = character(), estimate = numeric(),
                      lower = numeric(), upper = numeric())
  if (!length(terms)) return(empty)
  keys <- vapply(terms, .term_key, character(1))
  groups <- split(seq_along(terms), keys)
  group_estimates <- vapply(groups, function(idx) mean(coefs[terms[idx]]), numeric(1))
  limits <- matrix(NA_real_, length(groups), 2L,
                   dimnames = list(names(groups), c("lower", "upper")))
  n <- explanation$neighborhood

  if (ci_method == "normal") {
    # model.matrix() provides the basis functions used in the fitted model.
    # Do not index model$bx again: its columns are already selected.
    design <- as.matrix(stats::model.matrix(explanation$model))
    if (nrow(design) != length(n$y) || ncol(design) != length(coefs))
      stop("The MARS basis matrix does not match the fitted coefficients")
    # Ignore spaces when comparing MARS term names.
    matrix_names <- gsub("\\s+", "", colnames(design))
    coef_names <- gsub("\\s+", "", names(coefs))
    if (!identical(matrix_names, coef_names))
      stop("The MARS basis terms are not in coefficient order")
    w <- n$weights
    fit_y <- as.numeric(design %*% unname(coefs))
    weighted_design <- design * sqrt(w)
    rank <- qr(weighted_design)$rank
    df <- nrow(design) - rank
    if (df > 0L && rank == ncol(design)) {
      sigma2 <- sum(w * (n$y - fit_y)^2) / df
      covariance <- tryCatch(solve(crossprod(weighted_design)) * sigma2,
                             error = function(e) NULL)
      if (!is.null(covariance)) {
        for (key in names(groups)) {
          ids <- match(terms[groups[[key]]], names(coefs))
          # Var(mean(beta)) = sum(Cov(beta)) / length(beta)^2.
          variance <- sum(covariance[ids, ids, drop = FALSE]) / length(ids)^2
          se <- sqrt(max(variance, 0))
          limits[key, ] <- group_estimates[key] + c(-1, 1) * stats::qnorm(0.975) * se
        }
      }
    }
  } else {
    if (length(n_boot) != 1L || is.na(n_boot) || n_boot < 2L ||
        n_boot != as.integer(n_boot)) stop("n_boot must be an integer >= 2")
    draws <- matrix(NA_real_, n_boot, length(groups),
                    dimnames = list(NULL, names(groups)))
    for (b in seq_len(n_boot)) {
      ids <- sample.int(nrow(n$x), nrow(n$x), replace = TRUE)
      fit <- tryCatch(.mars_fit(n$x[ids, , drop = FALSE], n$y[ids],
                                n$weights[ids], explanation$degree),
                      error = function(e) NULL)
      if (is.null(fit)) next
      boot_coef <- tryCatch(.group_coefficients(.coefficient_vector(fit)),
                            error = function(e) NULL)
      if (is.null(boot_coef)) next
      common <- intersect(names(groups), names(boot_coef))
      draws[b, common] <- boot_coef[common]
    }
    for (key in names(groups)) {
      values <- draws[is.finite(draws[, key]), key]
      if (length(values) >= 2L)
        limits[key, ] <- as.numeric(stats::quantile(values, c(0.025, 0.975),
                                                      names = FALSE))
    }
  }
  data.frame(term = names(groups), estimate = unname(group_estimates),
             lower = limits[, 1L], upper = limits[, 2L], row.names = NULL)
}

#' Coefficients Stability Index (CSI)
#'
#' Computes pairwise confidence-interval overlap for grouped MARS coefficients.
#' Terms are matched by variables, hinge directions, and interaction structure,
#' ignoring knot values. Multiple coefficients in a group are averaged.
#' The index is the mean overlap percentage across comparable groups.
#'
#' @param records Repeated explanation objects containing coefficient_ci tables.
#' @param ci_method "normal" or "bootstrap" (percentile bootstrap intervals).
#' @return CSI percentage, or NA if no valid comparisons exist.
#' @export
aplime_csi <- function(records, ci_method = c("normal", "bootstrap")) {
  ci_method <- match.arg(ci_method)
  if (length(records) < 2L) return(NA_real_)
  intervals <- lapply(records, function(r) {
    if (!is.null(r$coefficient_ci)) r$coefficient_ci[[ci_method]] else NULL
  })
  if (any(vapply(intervals, is.null, logical(1)))) return(NA_real_)
  all_keys <- unique(unlist(lapply(intervals, function(d) d$term)))
  partial <- numeric()
  for (key in all_keys) {
    present <- which(vapply(intervals, function(d) key %in% d$term, logical(1)))
    if (length(present) < 2L) next
    pairs <- utils::combn(present, 2L)
    overlaps <- numeric()
    for (j in seq_len(ncol(pairs))) {
      a <- intervals[[pairs[1L, j]]]
      b <- intervals[[pairs[2L, j]]]
      a <- a[a$term == key, , drop = FALSE]
      b <- b[b$term == key, , drop = FALSE]
      if (!all(is.finite(c(a$lower, a$upper, b$lower, b$upper)))) next
      overlaps <- c(overlaps, as.numeric(max(a$lower, b$lower) <=
                                         min(a$upper, b$upper)))
    }
    if (length(overlaps)) partial <- c(partial, mean(overlaps))
  }
  if (!length(partial)) return(NA_real_)
  100 * mean(partial)
}

#' List Original Hinge Terms and Thresholds Used in CSI
#'
#' Each row identifies a repetition, the knot-insensitive CSI group key, the
#' original MARS term, its unaveraged coefficient, and hinge thresholds.
#' `different_knots` flags groups matched despite differing thresholds.
#' CSI estimates use group means, not the individual coefficients shown here.
#'
#' @param records List of repeated AP-LIME/AP2-LIME explanation objects.
#' @return Data frame with repetition, term_group, original_term, hinges,
#'   coefficient, and different_knots. An empty data frame means no hinge terms.
#' @export
aplime_knot_report <- function(records) {
  rows <- lapply(seq_along(records), function(i) {
    b <- .coefficient_vector(records[[i]]$model)
    b <- b[names(b) != "(Intercept)" & is.finite(b)]
    if (!length(b)) return(NULL)
    data.frame(repetition = i,
               term_group = vapply(names(b), .term_key, character(1)),
               original_term = names(b),
               hinges = vapply(names(b), .term_knots, character(1)),
               coefficient = unname(b), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  empty <- data.frame(repetition = integer(), term_group = character(),
                      original_term = character(), hinges = character(),
                      coefficient = numeric(), different_knots = logical())
  if (!length(rows)) return(empty)
  result <- do.call(rbind, rows)
  distinct <- tapply(result$original_term, result$term_group,
                     function(x) length(unique(x)) > 1L)
  result$different_knots <- as.logical(distinct[result$term_group])
  rownames(result) <- NULL
  result
}

#' Variable Selection Stability Index
#'
#' @param records List of repeated explanation objects.
#' @param n_features Number of selected features; defaults to each object's top-feature count.
#' @return Mean pairwise percentage of common selected features, or `NA` if too few runs.
#' @export
aplime_vsi <- function(records, n_features = NULL) {
  if (length(records) < 2L) return(NA_real_)
  if (is.null(n_features)) n_features <- length(records[[1L]]$top_features)
  if (length(n_features) != 1L || n_features < 1L) stop("invalid n_features")
  pairs <- utils::combn(seq_along(records), 2L)
  scores <- apply(pairs, 2L, function(pair) {
    length(intersect(records[[pair[1L]]]$top_features,
                     records[[pair[2L]]]$top_features)) / n_features
  })
  100 * mean(scores)
}

#' Repeated AP-LIME or AP2-LIME Explanations
#'
#' @param explainer An `aplime_explainer`.
#' @param instance One observation to explain.
#' @param degree Interaction degree 1 or 2.
#' @param n_repeats Number of repeated explanations (at least 2).
#' @param n_samples Number of perturbations per repeat.
#' @param n_features Number of top-ranked features (default: up to 6, capped at the number of features).
#' @param n_boot Bootstrap replications; 0 disables bootstrap CSI, but normal CSI is always computed.
#' @param seed Optional seed.
#' @return List with runs, VSI, coefficient CSI, rank frequencies, and
#'   `knot_report` (all original hinges/thresholds and differing-knot flags),
#'   `different_knot_groups` (only groups with differing thresholds).
#'   Group coefficients are arithmetic means, with corresponding confidence
#'   intervals. Differing knots generate a warning and remain in the reports.
#' @export
aplime_repeat <- function(explainer, instance, degree = 1L, n_repeats = 100L,
                          n_samples = 5000L, n_features = 6L, n_boot = 0L, seed = NULL) {
  if (length(n_repeats) != 1L || is.na(n_repeats) || n_repeats < 2L ||
      n_repeats != as.integer(n_repeats)) stop("n_repeats must be at least 2")
  if (length(n_boot) != 1L || is.na(n_boot) || n_boot < 0L ||
      (n_boot == 1L) || n_boot != as.integer(n_boot)) stop("n_boot must be zero or at least 2")
  if (missing(n_features)) n_features <- min(6L, length(.instance(explainer, instance)))
  if (!is.null(seed)) set.seed(seed)
  runs <- vector("list", n_repeats)
  for (i in seq_len(n_repeats)) {
    run <- aplime_explain(explainer, instance, degree, n_samples,
                          n_features, keep_neighborhood = TRUE)
    # Select the strongest coefficient groups for CSI.
    coefficients <- .coefficient_vector(run$model)
    basis <- coefficients[names(coefficients) != "(Intercept)"]
    grouped <- .group_coefficients(basis)
    selected_groups <- head(names(sort(abs(grouped), decreasing = TRUE)), n_features)
    normal_ci <- .coefficient_intervals(run, "normal")
    normal_ci <- normal_ci[normal_ci$term %in% selected_groups, , drop = FALSE]
    boot_ci <- if (n_boot > 0L) .coefficient_intervals(run, "bootstrap", n_boot) else NULL
    if (!is.null(boot_ci)) boot_ci <- boot_ci[boot_ci$term %in% selected_groups, , drop = FALSE]
    run$coefficient_ci <- list(normal = normal_ci, bootstrap = boot_ci)
    run$neighborhood <- NULL
    runs[[i]] <- run
  }
  positions <- do.call(rbind, lapply(seq_along(runs), function(i) {
    data.frame(repetition = i, rank = seq_along(runs[[i]]$top_features),
               feature = names(runs[[i]]$contributions)[runs[[i]]$top_features])
  }))
  frequency <- as.data.frame(table(positions$rank, positions$feature), stringsAsFactors = FALSE)
  names(frequency) <- c("rank", "feature", "count")
  frequency$percentage <- 100 * frequency$count / n_repeats
  frequency <- frequency[frequency$count > 0L, , drop = FALSE]
  knot_report <- aplime_knot_report(runs)
  # Report thresholds for coefficient groups included in CSI.
  selected_keys <- unique(unlist(lapply(runs, function(r) r$coefficient_ci$normal$term)))
  knot_report <- knot_report[knot_report$term_group %in% selected_keys, , drop = FALSE]
  differing <- knot_report[knot_report$different_knots, , drop = FALSE]
  if (nrow(differing)) {
    warning(length(unique(differing$term_group)),
            " coefficient group(s) had different knot thresholds. ",
            "CSI matched these terms by name/direction, ignoring knots. ",
            "See $different_knot_groups and $knot_report for all threshold values.",
            call. = FALSE)
  }
  list(runs = runs, vsi_percent = aplime_vsi(runs, n_features),
       csi_normal_percent = aplime_csi(runs, "normal"),
       csi_bootstrap_percent = if (n_boot > 0L) aplime_csi(runs, "bootstrap") else NA_real_,
       rank_frequency = frequency, knot_report = knot_report,
       different_knot_groups = differing)
}

#' Create a Prediction Function from an XGBoost Model
#'
#' @param model A fitted `xgb.Booster` model or path to a serialized model.
#' @return Function suitable for `predict_function` in [aplime_explainer()].
#' @export
aplime_predict_xgboost <- function(model) {
  if (!requireNamespace("xgboost", quietly = TRUE))
    stop("Install the optional 'xgboost' package")
  if (is.character(model) && length(model) == 1L)
    model <- xgboost::xgb.load(model)
  force(model)
  function(new_x) as.numeric(stats::predict(model, as.matrix(new_x)))
}

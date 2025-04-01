#' @title PCA explained
#' @name mlr_pipeops_pca_explained
#'
#' @description
#' PipeOpPCAExplained performs PCA on the data and returns the scores that explain a certain proportion of the variance.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc],
#' as well as the following parameters:
#' * `center` :: `logical(1)`\cr
#' Logical indicating whether the variables should be centered.
#' Default is `FALSE`.
#' * `scale.` :: `logical(1)`\cr
#' Logical indicating whether the variables should be scaled.
#' Default is `FALSE`.
#' * `var.` :: `numeric(1)`\cr
#' A numeric value indicating the proportion of variance to explain.
#' Default is `NULL`.
#'
#' @export PipeOpPCAExplained
PipeOpPCAExplained = R6::R6Class(
  "PipeOpPCAExplained",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    #' @description Constructor.
    #' @param id (`character(1)`)\cr
    #'   Identifier of resulting object, default `"drop.nacol"`.
    #' @param param_vals (named `list()`)\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would
    #'   otherwise be set during construction. Default `list()`.
    initialize = function(id = "pca_explained", param_vals = list()) {
      ps = ps(
        center = p_lgl(default = FALSE, tags = c("train", "pca")),
        scale. = p_lgl(default = FALSE, tags = c("train", "pca")),
        var. = p_dbl(lower = 0, upper = 0.999, default = NULL, tags = c("train"), special_vals = list(NULL))
      )
      ps$values = list(center = FALSE, scale. = FALSE)
      super$initialize(
        id,
        param_set = ps,
        param_vals = param_vals,
        feature_types = c("numeric", "integer")
      )
    }
  ),
  private = list(
    .train_dt = function(dt, levels, target) {
      pv = self$param_set$values
      pcr = mlr3misc::invoke(stats::prcomp,
                             as.matrix(dt),
                             .args = self$param_set$get_values(tags = "pca"))
      cumulative_proportion <- cumsum(pcr$sdev^2) / sum(pcr$sdev^2)
      n_components <- which(cumulative_proportion >= pv$var.)[1]
      pcr <- prcomp(as.matrix(dt),
                    center = pv$center,
                    rank. = n_components)
      self$state = pcr
      self$state$x = NULL

      # save scores
      # dir_name = "./pcr"
      # if (!dir.exists(dir_name)) {
      #   dir.create(dir_name)
      # }
      # random_id <- paste0(sample(0:9, 15, replace = TRUE), collapse = "")
      # file_name = paste0("pcr-", task$id, "-", random_id, ".rds")
      # file_name = file.path(dir_name, file_name)
      # saveRDS(pcr, file_name)

      # self$state$n_components = n_components
      pcr$x
    },

    .predict_dt = function(dt, levels) {
      stats::predict(self$state, as.matrix(dt))
    }
  )
)

#' @include zzz.R
register_po("pca_explained", PipeOpPCAExplained)

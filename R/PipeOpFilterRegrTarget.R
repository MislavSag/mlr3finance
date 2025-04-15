#' @title Filter Rows Based on Extreme Target Values
#' @name mlr_pipeops_filterregrtarget
#'
#' @description
#' This PipeOp filters the training set for a regression task by retaining only those observations
#' with target values that are extreme. The user can specify lower and upper percentile thresholds;
#' only rows with a target value less than or equal to the lower threshold or greater than or equal to
#' the upper threshold are retained. No filtering is applied during prediction.
#'
#' @section Parameters:
#' The parameters are inherited from [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc],
#' as well as the following additional parameters:
#' * `lower_percentile` :: `numeric(1)`\cr
#'   The quantile value for the lower threshold. Only rows with target values below (or equal to)
#'   this value are kept. Default is `0.1` (10th percentile).
#' * `upper_percentile` :: `numeric(1)`\cr
#'   The quantile value for the upper threshold. Only rows with target values above (or equal to)
#'   this value are kept. Default is `0.9` (90th percentile).
#'
#' @export PipeOpFilterRegrTarget
PipeOpFilterRegrTarget = R6::R6Class(
  "PipeOpFilterRegrTarget",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    #' @description Constructor.
    #' @param id (`character(1)`)\cr
    #'   Identifier of the resulting object, default `"filter_target"`.
    #' @param param_vals (named `list()`)\cr
    #'   List of hyperparameter settings. Default is `list()`.
    initialize = function(id = "filter_target", param_vals = list()) {
      ps = ps(
        lower_percentile = p_dbl(
          default = 0.1,
          lower = 0,
          upper = 1,
          tags = "filter_target"
        ),
        upper_percentile = p_dbl(
          default = 0.9,
          lower = 0,
          upper = 1,
          tags = "filter_target"
        )
      )
      ps$values = list(lower_percentile = 0.1, upper_percentile = 0.9)
      # We do not require any specific feature types here because the operator operates on the task's target.
      super$initialize(
        id,
        param_set = ps,
        param_vals = param_vals,
        feature_types = character(0)
      )
    }
  ),
  private = list(
    .train_task = function(task) {
      params = self$param_set$get_values(tags = "filter_target")
      # Extract full data of the task.
      data = task$data()
      # Compute the quantile thresholds for the target variable.
      target_q = quantile(data[[task$target_names]],
                          probs = c(params$lower_percentile, params$upper_percentile))
      # Create a logical index: TRUE for rows where target is at or below the lower threshold OR at or above the upper threshold.
      include = data[[task$target_names]] <= target_q[1] |
        data[[task$target_names]] >= target_q[2]
      # Get the row ids that satisfy the condition.
      filtered_row_ids = task$row_ids[include]
      # Filter the task to keep only the selected rows.
      task$filter(filtered_row_ids)
      return(task)
    },
    .predict_task = function(task) {
      # No filtering is applied during prediction.
      task
    }
  )
)

#' @include zzz.R
register_po("filterregrtarget", PipeOpFilterRegrTarget)

#' @title Drop rows with missing values
#' @name mlr_pipeops_dropna
#'
#' @description
#' PipeOpDropNA removes rows with missing values from the task.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`][mlr3pipelines::PipeOpTaskPreproc]
#'
#' @export PipeOpDropNA
PipeOpDropNA = R6::R6Class(
  "PipeOpDropNA",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    #' @description Constructor.
    #' @param id (`character(1)`)\cr
    #'   Identifier of resulting object, default `"drop.nacol"`.
    #' @param param_vals (named `list()`)\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would
    #'   otherwise be set during construction. Default `list()`.
    initialize = function(id = "drop.na") {
      super$initialize(id)
    }
  ),

  private = list(
    .train_task = function(task) {
      self$state = list()
      private$compute_exclude(task)
    },

    .predict_task = function(task) {
      private$compute_exclude(task)
    },

    compute_exclude = function(task) {
      featuredata = task$data(cols = task$feature_names)
      exclude = apply(is.na(featuredata), 1, any)
      task$filter(task$row_ids[!exclude])
    }
  )
)

#' @include zzz.R
register_po("dropna", PipeOpDropNA)

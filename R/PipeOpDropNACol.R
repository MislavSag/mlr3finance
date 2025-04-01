#' @title Drop NA Columns
#' @name mlr_pipeops_dropnacol
#'
#' @description
#' Drops columns with more than a certain fraction of missing values.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`][mlr3pipelines::PipeOpTaskPreprocSimple],
#' as well as the following parameters:
#' * `cutoff` :: `numeric(1)`\cr
#'  Fraction of missing values above which a column is dropped.
#'  Default is `0.05`.
#'
#' @export PipeOpDropNACol
PipeOpDropNACol = R6::R6Class(
  "PipeOpDropNACol",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    #' @description Constructor.
    #' @param id (`character(1)`)\cr
    #'   Identifier of resulting object, default `"drop.nacol"`.
    #' @param param_vals (named `list()`)\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would
    #'   otherwise be set during construction. Default `list()`.
    initialize = function(id = "drop.nacol", param_vals = list()) {
      ps = ps(
        cutoff = p_dbl(default = 0.05, lower = 0, upper = 1, tags = c("dropnacol_tag"))
      )
      ps$values = list(cutoff = 0.2)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    }
  ),

  private = list(
    .get_state = function(task) {
      pv = self$param_set$get_values(tags = "dropnacol_tag")
      features_names = task$feature_names
      data = task$data(cols = features_names)
      keep = sapply(data, function(column) (sum(is.na(column))) / length(column) < pv$cutoff)
      list(cnames = colnames(data)[keep])
    },

    .transform = function(task) {
      task$select(self$state$cnames)
    }
  )
)

#' @include zzz.R
register_po("drop.nacol", PipeOpDropNACol)

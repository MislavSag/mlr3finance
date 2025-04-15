#' @title Impute NA and/or Inf Values via LOCF using setnafill
#' @name mlr_pipeops_imputelocf
#'
#' @description
#' This PipeOp imputes missing values in numeric features using last observation carried forward (LOCF)
#' imputation via data.table's `setnafill()`. It can be configured to impute:
#' \itemize{
#'   \item Only `NA` values (leaving infinite values untouched),
#'   \item Only infinite (`Inf` or `-Inf`) values (imputing only positions that are infinite),
#'   \item Both `NA` and infinite values.
#' }
#' For the `"inf"` option, only positions with infinite values are imputed (original `NA` values remain unchanged).
#'
#' @section Parameters:
#' The parameters are inherited from [`PipeOpTaskPreprocSimple`][mlr3pipelines::PipeOpTaskPreprocSimple],
#' plus the following:
#' * `impute_on` :: `character(1)`\cr
#'   Determines which values to impute. Allowed values are `"na"`, `"inf"`, or `"both"`. Default is `"both"`.
#'
#' @export PipeOpImputeLocf
PipeOpImputeLocf = R6::R6Class(
  "PipeOpImputeLocf",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    #' @description Constructor.
    #' @param id (`character(1)`)\cr
    #'   Identifier of the resulting object, default `"imputelocf"`.
    #' @param param_vals (named `list()`)\cr
    #'   List of hyperparameter settings. Default is `list()`.
    initialize = function(id = "imputelocf", param_vals = list()) {
      ps = ps(impute_on = p_fct(
        levels = c("na", "inf", "both"),
        default = "both",
        tags = "imputelocf"
      ))
      ps$values = list(impute_on = "both")
      super$initialize(
        id = id,
        param_set = ps,
        param_vals = param_vals,
        feature_types = c("numeric", "integer")
      )
    }
  ),
  private = list(
    # Select only numeric (and integer) feature columns.
    .select_cols = function(task) {
      task$feature_types[type %in% c("numeric", "integer"), "id"][[1L]]
    },

    .get_state = function(task) {
      list()  # This operator is stateless.
    },

    # Apply the transformation:
    # - If impute_on == "both": Replace Inf with NA and impute entire column.
    # - If impute_on == "inf": Impute only positions that are infinite (leaving existing NA unchanged).
    # - If impute_on == "na": Impute only originally missing values.
    .transform = function(task) {
      numeric_cols = private$.select_cols(task)
      if (length(numeric_cols) == 0) {
        return(task)
      }
      dt = task$data(cols = numeric_cols)
      impute_on = self$param_set$values$impute_on

      if (impute_on == "both") {
        dt[, (numeric_cols) := lapply(.SD, function(x) {
          x[is.infinite(x)] <- NA
          nafill(x, type = "locf")
        }), .SDcols = numeric_cols]
      } else if (impute_on == "inf") {
        dt[, (numeric_cols) := lapply(.SD, function(x) {
          idx_inf = which(is.infinite(x))
          if (length(idx_inf) > 0) {
            x_copy = x
            x_copy[idx_inf] <- NA
            x_imputed = setnafill(x_copy, type = "locf")
            x[idx_inf] = x_imputed[idx_inf]
          }
          x
        }), .SDcols = numeric_cols]
      } else if (impute_on == "na") {
        dt[, (numeric_cols) := lapply(.SD, function(x) {
          setnafill(x, type = "locf")
        }), .SDcols = numeric_cols]
      }

      task$select(setdiff(task$feature_names, numeric_cols))$cbind(dt)
      return(task)
    }
  )
)

#' @include zzz.R
register_po("imputelocf", PipeOpImputeLocf)

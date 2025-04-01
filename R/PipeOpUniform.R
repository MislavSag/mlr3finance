#' @title Uniformize Numeric Features via ECDF
#' @name mlr_pipeops_uniform
#'
#' @description
#' This PipeOp transforms numeric features by applying the empirical cumulative distribution
#' function (ECDF) to each predictor. This maps the feature values to the \[0, 1\] interval.
#' If a grouping variable is defined in the task (via the group role), uniformization is applied
#' separately within each group.
#'
#' @section Parameters:
#' The parameters are inherited from [`PipeOpTaskPreprocSimple`][mlr3pipelines::PipeOpTaskPreprocSimple],
#' as well as the following parameter:
#' * `na.rm` :: `logical(1)`\cr
#'   Whether to ignore missing values when computing the ECDF. Default is `TRUE`.
#'
#' @export PipeOpUniform
PipeOpUniform = R6::R6Class(
  "PipeOpUniform",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    #' @description Constructor.
    #' @param id (`character(1)`)\cr
    #'   Identifier of resulting object, default `"uniformization"`.
    #' @param param_vals (named `list()`)\cr
    #'   List of hyperparameter settings. Default is `list()`.
    initialize = function(id = "uniformization", param_vals = list()) {
      ps = ps(na.rm = p_lgl(default = TRUE, tags = "uniformize"))
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

    # Compute and store the ECDF functions for each numeric feature.
    # If a grouping column exists, compute ECDFs separately by group.
    .get_state = function(task) {
      numeric_cols = private$.select_cols(task)
      if (length(numeric_cols) == 0) {
        return(list(cols = numeric_cols))
      }
      data_dt = task$data(cols = numeric_cols)
      group_col = NULL
      if (length(task$col_roles$groups)) {
        group_col = task$col_roles$groups[[1L]]
        group_dt = task$backend$data(
          rows = task$row_ids,
          cols = c(task$backend$primary_key, group_col)
        )
        stopifnot(all(group_dt[[task$backend$primary_key]] == task$row_ids))
        data_dt[[group_col]] = group_dt[[group_col]]
      }
      na_ignore = self$param_set$get_values()$`na.rm`
      state_list = list()
      if (!is.null(group_col)) {
        # Compute an ECDF for each numeric feature within each group.
        for (col in numeric_cols) {
          ecdf_by_group = list()
          groups = unique(data_dt[[group_col]])
          for (g in groups) {
            vals = data_dt[[col]][data_dt[[group_col]] == g]
            if (na_ignore) {
              vals = vals[!is.na(vals)]
            }
            ecdf_by_group[[as.character(g)]] = ecdf(vals)
          }
          state_list[[col]] = ecdf_by_group
        }
        state_list$cols = numeric_cols
        state_list$group_col = group_col
      } else {
        # Compute a global ECDF for each numeric feature.
        for (col in numeric_cols) {
          vals = data_dt[[col]]
          if (na_ignore) {
            vals = vals[!is.na(vals)]
          }
          state_list[[col]] = ecdf(vals)
        }
        state_list$cols = numeric_cols
        state_list$group_col = NULL
      }
      return(state_list)
    },

    # Apply the stored ECDF functions to transform the data.
    .transform = function(task) {
      numeric_cols = self$state$cols
      if (length(numeric_cols) == 0) {
        return(task)
      }
      data_dt = task$data(cols = numeric_cols)
      if (!is.null(self$state$group_col)) {
        group_col = self$state$group_col
        group_dt = task$backend$data(
          rows = task$row_ids,
          cols = c(task$backend$primary_key, group_col)
        )
        stopifnot(all(group_dt[[task$backend$primary_key]] == task$row_ids))
        data_dt[[group_col]] = group_dt[[group_col]]
        # Transform each feature by applying the corresponding group-specific ECDF.
        for (col in numeric_cols) {
          groups = unique(data_dt[[group_col]])
          for (g in groups) {
            idx = data_dt[[group_col]] == g & !is.na(data_dt[[col]])
            ecdf_fun = self$state[[col]][[as.character(g)]]
            if (!is.null(ecdf_fun)) {
              data_dt[[col]][idx] = ecdf_fun(data_dt[[col]][idx])
            }
          }
        }
        data_dt[, (group_col) := NULL]
      } else {
        # Global uniformization.
        for (col in numeric_cols) {
          ecdf_fun = self$state[[col]]
          idx = !is.na(data_dt[[col]])
          data_dt[[col]][idx] = ecdf_fun(data_dt[[col]][idx])
        }
      }
      task$select(setdiff(task$feature_names, numeric_cols))$cbind(data_dt)
      return(task)
    }
  )
)

#' @include zzz.R
register_po("uniform", PipeOpUniform)

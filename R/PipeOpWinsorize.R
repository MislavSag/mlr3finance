# Define the custom Winsorization PipeOp using sugar functions for parameters
#' @title Winsorize Numeric Features
#' @name mlr_pipeops_winsorize
#'
#' @description
#' This PipeOp performs winsorization on numeric features by capping values below the lower
#' percentile and above the upper percentile. If a grouping variable is defined in the task
#' (via the group role), the winsorization is applied separately within each group.
#'
#' @section Parameters:
#' The parameters are inherited from [`PipeOpTaskPreprocSimple`][mlr3pipelines::PipeOpTaskPreprocSimple],
#' as well as the following parameters:
#' * `lower` :: `numeric(1)`\cr
#'   Lower percentile bound (between 0 and 1). Default is `0.01`.
#' * `upper` :: `numeric(1)`\cr
#'   Upper percentile bound (between 0 and 1). Default is `0.99`.
#' * `na.rm` :: `logical(1)`\cr
#'   Whether to remove missing values when computing quantiles. Default is `TRUE`.
#'
#' @export PipeOpWinsorize
PipeOpWinsorize = R6::R6Class(
  "PipeOpWinsorize",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  # inherit from TaskPreprocSimple for easy state handling
  public = list(
    #' @description Constructor.
    #' @param id (`character(1)`)\cr
    #'   Identifier of resulting object, default `"winsorize"`.
    #' @param param_vals (named `list()`)\cr
    #'   List of hyperparameter settings. Default is `list()`.
    initialize = function(id = "winsorize", param_vals = list()) {
      ps = ps(
        lower = p_dbl(
          lower = 0,
          upper = 1,
          default = 0.01,
          tags = "winsorize"
        ),
        upper = p_dbl(
          lower = 0,
          upper = 1,
          default = 0.99,
          tags = "winsorize"
        ),
        na.rm = p_lgl(default = TRUE, tags = "winsorize")
      )
      # Set default values explicitly, if needed
      ps$values = list(lower = 0.01,
                       upper = 0.99,
                       na.rm = TRUE)
      super$initialize(
        id = id,
        param_set = ps,
        param_vals = param_vals,
        feature_types = c("numeric", "integer")
      )
    }
  ),
  private = list(
    # Select only numeric (and integer) feature columns for processing
    .select_cols = function(task) {
      task$feature_types[type %in% c("numeric", "integer"), "id"][[1L]]
    },

    # Compute and store the winsorization thresholds (state) during training
    .get_state = function(task) {
      # Identify numeric feature columns to process
      numeric_cols = private$.select_cols(task)
      if (!length(numeric_cols)) {
        return(list(cols = numeric_cols))  # no numeric features, nothing to do
      }
      # Extract the data for numeric columns
      data_dt = task$data(cols = numeric_cols)

      # Check if a grouping variable is defined in the task
      group_col = NULL
      if (length(task$col_roles$groups)) {
        group_col = task$col_roles$groups[[1L]]  # (assuming a single grouping column)
        # Append the group values to the data (aligning by row_id)
        group_dt = task$backend$data(
          rows = task$row_ids,
          cols = c(task$backend$primary_key, group_col)
        )
        stopifnot(all(group_dt[[task$backend$primary_key]] == task$row_ids))  # ensure alignment
        data_dt[[group_col]] = group_dt[[group_col]]
      }

      # Get parameter values for percentile bounds
      params = self$param_set$get_values(tags = "winsorize")
      low_prob = params$lower
      high_prob = params$upper
      if (low_prob > high_prob) {
        stop(
          sprintf(
            "PipeOpWinsorize: 'lower' percentile (%.2f) is greater than 'upper' (%.2f)",
            low_prob,
            high_prob
          )
        )
      }
      na_ignore = params$`na.rm`

      # Calculate quantile thresholds
      if (!is.null(group_col)) {
        # Group-wise quantiles: compute per group for each numeric column
        quantiles_by_group = list()
        # Loop over numeric columns to compute group-wise quantiles
        for (col in numeric_cols) {
          # Use data.table to compute quantiles within each group for the column
          q_dt = data_dt[, .(q_low = as.numeric(quantile(
            get(col), probs = low_prob, na.rm = na_ignore
          )),
          q_high = as.numeric(quantile(
            get(col), probs = high_prob, na.rm = na_ignore
          ))), by = get(group_col)]
          setnames(q_dt, "get", group_col)  # rename the grouping key column
          # Store the thresholds table for this feature
          quantiles_by_group[[col]] = q_dt
        }
        # Store state: thresholds for each column by group
        state = list(cols = numeric_cols,
                     group_col = group_col,
                     group_quantiles = quantiles_by_group)
      } else {
        # Global quantiles (no grouping)
        low_vals = numeric()
        high_vals = numeric()
        # Compute thresholds for each numeric column
        for (col in numeric_cols) {
          low_vals[col] = as.numeric(quantile(data_dt[[col]], probs = low_prob, na.rm = na_ignore))
          high_vals[col] = as.numeric(quantile(data_dt[[col]], probs = high_prob, na.rm = na_ignore))
        }
        state = list(
          cols = numeric_cols,
          group_col = NULL,
          low_vals = low_vals,
          high_vals = high_vals
        )
      }
      return(state)
    },

    # Apply winsorization to a Task using the stored state (for both training and prediction)
    .transform = function(task) {
      numeric_cols = self$state$cols
      if (!length(numeric_cols)) {
        return(task)  # nothing to transform
      }
      # Extract data for numeric columns to transform
      data_dt = task$data(cols = numeric_cols)

      if (!is.null(self$state$group_col)) {
        # Group-wise transformation
        group_col = self$state$group_col
        # Attach group values to data
        group_dt = task$backend$data(
          rows = task$row_ids,
          cols = c(task$backend$primary_key, group_col)
        )
        stopifnot(all(group_dt[[task$backend$primary_key]] == task$row_ids))
        data_dt[[group_col]] = group_dt[[group_col]]
        # Loop through each numeric column and cap values per group
        for (col in numeric_cols) {
          # Get quantile table for this column (stored during training)
          q_dt = self$state$group_quantiles[[col]]
          # Ensure it’s keyed by group for fast joins (if not already)
          setkeyv(q_dt, group_col)
          # Join with data on group to retrieve matching quantiles
          data_dt = q_dt[data_dt, on = group_col][, (col) := {
            # Use the joined thresholds to cap the column values
            x = get(col)
            low = q_low[1L]
            high = q_high[1L]  # q_low and q_high are brought in by join per group
            pmin(pmax(x, low), high)
          }, by = .EACHI]
          # Remove the threshold columns after applying (to keep data_dt clean)
          data_dt[, c("q_low", "q_high") := NULL]
        }
        # Drop the grouping column from the data table (it’s not a feature)
        data_dt[, (group_col) := NULL]
      } else {
        # Global winsorization (no grouping)
        for (col in numeric_cols) {
          # Apply the precomputed global thresholds
          low = self$state$low_vals[col]
          high = self$state$high_vals[col]
          data_dt[[col]] = pmin(pmax(data_dt[[col]], low), high)
        }
      }

      # Construct the new output Task with transformed data
      task$select(setdiff(task$feature_names, numeric_cols))$cbind(data_dt)
      return(task)
    }
  )
)

#' @include zzz.R
register_po("winsorize", PipeOpWinsorize)

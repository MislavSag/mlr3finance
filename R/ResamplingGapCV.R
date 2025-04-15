#' @title Rolling and Expanding Gap Cross Validation
#' @name mlr_resamplings_gap_cv
#'
#' @description
#' Creates a rolling or expanding gap cross-validation resampling strategy.
#' This resampling strategy is designed for time series data, where the training set is defined by an initial window,
#' a gap, and a horizon. The gap is the number of observations skipped between the training and test sets.
#' The horizon is the number of observations in the test set.
#' If a group column is defined in the task (e.g. month), then the initial window, gap, and horizon are applied on the unique groups,
#' and the groups are subsequently mapped back to the row indices.
#'
#' @section Parameters:
#' The parameters are:
#' * `initial_window` :: `integer(1)`\cr
#'  The size of the initial training window. Default is `10`.
#'  * `horizon` :: `integer(1)`\cr
#'  The size of the test set (horizon). Default is `5`.
#'  * `gap` :: `integer(1)`\cr
#'  The number of observations to skip between the training and test sets. Default is `0`.
#'  * `step` :: `integer(1)`\cr
#'  The number of observations to slide the training window forward for each fold. Default is `1`.
#'  * `rolling` :: `logical(1)`\cr
#'  Whether to use a rolling (fixed-size) window or an expanding window. Default is `FALSE`.
#'
#' @export
ResamplingGapCV = R6::R6Class(
  "ResamplingGapCV",
  inherit = mlr3::Resampling,
  public = list(
    #' @description
    #' Create an gap CV instance.
    #'
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "gap_cv") {
      ps = ps(
        initial_window = p_int(lower = 1, tags = "required"),
        horizon        = p_int(lower = 1, tags = "required"),
        gap            = p_int(lower = 0, tags = "required"),
        step           = p_int(lower = 1, tags = "required"),
        rolling        = p_lgl(tags = "required")
      )
      # set some default values (can be overridden)
      ps$values = list(
        initial_window = 10,
        horizon = 5,
        gap = 0,
        step = 1,
        rolling = FALSE
      )

      super$initialize(
        id = id,
        param_set = ps,
        label = "Gap Walk-Forward CV",
        man = NA_character_
      )
    }
  ),
  active = list(
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations, depending on the
    #'   values stored in the `param_set`.
    iters = function() {
      if (!self$is_instantiated) {
        return(NA_integer_)
      }
      length(self$instance$train)
    }
  ),
  private = list(
    .sample = function(ids, ...) {
      # Determine if a group column is present (e.g., month)
      task = self$task  # available once instantiate() is called
      has_group = length(task$col_roles$group) > 0

      if (has_group) {
        # Use the first defined group column
        group_col = task$col_roles$group[[1]]
        # Extract group vector; assume data are ordered in time already!
        data_groups = task$data(cols = group_col)[[group_col]]
        unique_groups = unique(data_groups)
        N = length(unique_groups)
      } else {
        # Without grouping, work on individual observations
        ids = sort(ids)
        N = length(ids)
      }

      init    = self$param_set$values$initial_window
      horizon = self$param_set$values$horizon
      gap     = self$param_set$values$gap
      step    = self$param_set$values$step
      rolling = self$param_set$values$rolling

      # Ensure there is enough data for at least one fold.
      if (init + gap + horizon > N) {
        stop(
          "Not enough observations (or groups) to create one fold; reduce 'initial_window', 'gap', or 'horizon'."
        )
      }

      train_splits = list()
      test_splits  = list()
      fold = 1
      repeat {
        if (!rolling) {
          # Expanding window: training always starts at the first group/observation
          train_start = 1
          train_end   = init + (fold - 1) * step
        } else {
          # Rolling (fixed-size) window: slide forward by step each fold
          train_start = 1 + (fold - 1) * step
          train_end   = train_start + init - 1
        }

        # If there isn't enough remaining data (or groups) for gap and horizon, break
        if (train_end + gap + horizon > N)
          break

        if (has_group) {
          # Determine the groups to include in train and test sets
          train_groups = unique_groups[train_start:train_end]
          test_groups  = unique_groups[(train_end + gap + 1):(train_end + gap + horizon)]
          # Map groups back to row indices in the task data (keeping order)
          all_groups = task$data(cols = task$col_roles$group[[1]])[[task$col_roles$group[[1]]]]
          train_idx = which(all_groups %in% train_groups)
          test_idx  = which(all_groups %in% test_groups)
        } else {
          train_idx = ids[train_start:train_end]
          test_start = train_end + gap + 1
          test_end   = train_end + gap + horizon
          test_idx   = ids[test_start:test_end]
        }

        train_splits[[fold]] = train_idx
        test_splits[[fold]]  = test_idx
        fold = fold + 1
      }

      list(train = train_splits, test = test_splits)
    },
    .get_train = function(i) {
      self$instance$train[[i]]
    },
    .get_test = function(i) {
      self$instance$test[[i]]
    }
  )
)

#' @include aaa.R
resamplings[["gap_cv"]] = ResamplingGapCV

#' @title Holdout with Ratio and Optional Gap Cross Validation
#'
#' @name mlr_resamplings_holdout_gap_ratio
#'
#' @description
#' Creates a single-split holdout resampling strategy where the training set is defined by a ratio of the data,
#' an optional gap is skipped after the training set, and the remaining observations form the test set.
#'
#' If a group column is defined in the task (e.g. month), then the ratio and gap are applied on the unique groups,
#' and the groups are subsequently mapped back to the row indices.
#'
#' @export
ResamplingHoldoutGapRatio = R6::R6Class(
  "ResamplingHoldoutGapRatio",
  inherit = mlr3::Resampling,
  public = list(
    #' @description
    #' Create a holdout with gap resampling instance.
    #'
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "holdout_gap_ratio") {
      ps = paradox::ps(
        ratio = paradox::p_dbl(lower = 0, upper = 1, tags = "required"),
        gap   = paradox::p_int(lower = 0, tags = "required")
      )
      # Set default values; these can be overridden by the user.
      ps$values = list(
        ratio = 0.7,
        gap   = 0
      )

      super$initialize(
        id = id,
        param_set = ps,
        label = "Holdout with Ratio and Gap",
        man = NA_character_
      )
    }
  ),
  active = list(
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations (always 1 for holdout).
    iters = function() {
      if (!self$is_instantiated)
        return(NA_integer_)
      length(self$instance$train)
    }
  ),
  private = list(
    .sample = function(ids, ...) {
      task = self$task
      # Check for a grouping column (e.g. "month")
      has_group = length(task$col_roles$group) > 0

      if (has_group) {
        # Use the first group column
        group_col = task$col_roles$group[[1]]
        data_groups = task$data(cols = group_col)[[group_col]]
        unique_groups = unique(data_groups)
        N = length(unique_groups)
      } else {
        ids = sort(ids)
        N = length(ids)
      }

      ratio = self$param_set$values$ratio
      gap   = self$param_set$values$gap

      # Compute the training set size
      train_size = floor(ratio * N)
      if (train_size < 1)
        stop("Training set size is less than 1. Increase ratio or add more data.")
      if (train_size + gap >= N)
        stop("Not enough observations (or groups) remain for a test set after applying the ratio and gap.")

      if (has_group) {
        # Use unique groups: first train_size groups for training,
        # skip the next gap groups, and use the remaining groups for testing.
        train_groups = unique_groups[1:train_size]
        test_groups  = unique_groups[(train_size + gap + 1):N]
        # Map groups back to row indices in the task.
        all_groups = task$data(cols = group_col)[[group_col]]
        train_idx = which(all_groups %in% train_groups)
        test_idx  = which(all_groups %in% test_groups)
      } else {
        train_idx = ids[1:train_size]
        test_idx = ids[(train_size + gap + 1):N]
      }

      list(train = list(train_idx), test = list(test_idx))
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
resamplings[["holdout_gap_ratio"]] = ResamplingHoldoutGapRatio

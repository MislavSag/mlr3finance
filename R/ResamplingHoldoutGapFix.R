#' @title Holdout with Gap (Fixed Split) Cross Validation
#'
#' @name mlr_resamplings_holdout_gap_fix
#'
#' @description
#' Creates a single-split holdout resampling strategy where the training set consists of the first
#' `initial_window` observations (or groups), then a gap of `gap` observations (or groups) is skipped,
#' and the next `horizon` observations (or groups) form the test set.
#'
#' If a group column is defined in the task (e.g., month), the splitting is done on the unique groups,
#' and the groups are mapped back to the row indices.
#'
#' @export
ResamplingHoldoutGapFix = R6::R6Class(
  "ResamplingHoldoutGapFix",
  inherit = mlr3::Resampling,
  public = list(
    #' @description
    #' Create a holdout with gap (fixed split) resampling instance.
    initialize = function() {
      ps = paradox::ps(
        initial_window = paradox::p_int(lower = 1, tags = "required"),
        gap            = paradox::p_int(lower = 0, tags = "required"),
        horizon        = paradox::p_int(lower = 1, tags = "required")
      )
      # Set default values; these can be overwritten by the user.
      ps$values = list(initial_window = 70,
                       gap = 5,
                       horizon = 25)

      super$initialize(
        id = "holdout_gap_fix",
        param_set = ps,
        label = "Holdout with Gap (Fixed Split)",
        man = NA_character_
      )
    }
  ),
  active = list(
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations (always 1 for a holdout).
    iters = function() {
      if (!self$is_instantiated)
        return(NA_integer_)
      length(self$instance$train)
    }
  ),
  private = list(
    .sample = function(ids, ...) {
      task = self$task
      # Check if a group column is defined (e.g., "month")
      has_group = length(task$col_roles$group) > 0

      if (has_group) {
        # Use the first defined group column
        group_col = task$col_roles$group[[1]]
        data_groups = task$data(cols = group_col)[[group_col]]
        unique_groups = unique(data_groups)
        N = length(unique_groups)
      } else {
        ids = sort(ids)
        N = length(ids)
      }

      initial_window = self$param_set$values$initial_window
      gap            = self$param_set$values$gap
      horizon        = self$param_set$values$horizon

      # Ensure that there are enough observations (or groups) to create a split.
      if (initial_window + gap + horizon > N) {
        stop(
          "Not enough observations (or groups) to create a holdout split with the given 'initial_window', 'gap', and 'horizon'."
        )
      }

      if (has_group) {
        # Select groups for training and testing.
        train_groups = unique_groups[1:initial_window]
        test_groups  = unique_groups[(initial_window + gap + 1):(initial_window + gap + horizon)]
        # Map groups back to row indices in the task.
        all_groups = task$data(cols = group_col)[[group_col]]
        train_idx = which(all_groups %in% train_groups)
        test_idx  = which(all_groups %in% test_groups)
      } else {
        # Without groups, use the ordered row indices.
        train_idx = ids[1:initial_window]
        test_start = initial_window + gap + 1
        test_end   = initial_window + gap + horizon
        test_idx = ids[test_start:test_end]
      }

      # Return a single split in mlr3 format.
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
resamplings[["holdout_gap_fix"]] = ResamplingHoldoutGapFix

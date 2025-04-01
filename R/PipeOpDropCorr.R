#' @title Drop correlated features
#' @name mlr_pipeops_dropcor
#'
#' @description
#' This PipeOp removes features that are highly correlated with other features.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`][mlr3pipelines::PipeOpTaskPreprocSimple],
#' as well as the following parameters:
#' * `use` :: `character(1)`\cr
#' A character string indicating how to handle missing values. Possible values are
#' `"everything"`, `"all.obs"`, `"complete.obs"`, `"na.or.complete"`, and `"pairwise.complete.obs"`.
#' Default is `"everything"`.
#' * `method` :: `character(1)`\cr
#' A character string indicating the method to compute the correlation. Possible values are
#' `"pearson"`, `"kendall"`, and `"spearman"`. Default is `"pearson"`.
#' * `cutoff` :: `numeric(1)`\cr
#' A numeric value indicating the correlation threshold above which features are considered
#' highly correlated. Default is `0.99`.
#'
#' @export PipeOpDropCorr
PipeOpDropCorr = R6::R6Class(
  "PipeOpDropCorr",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    #' @description Constructor.
    #' @param id (`character(1)`)\cr
    #'   Identifier of resulting object, default `"drop.nacol"`.
    #' @param param_vals (named `list()`)\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would
    #'   otherwise be set during construction. Default `list()`.
    initialize = function(id = "dropcor", param_vals = list()) {
      ps = ps(
        use = p_fct(levels = c("everything", "all.obs", "complete.obs",
                               "na.or.complete", "pairwise.complete.obs"),
                    default = "everything"),
        method = p_fct(levels = c("pearson", "kendall", "spearman"),
                       default = "pearson"),
        cutoff = p_dbl(lower = 0, upper = 1, default = 0.99)
      )
      ps$values = list(use = "everything", method = "pearson", cutoff = 0.99)
      super$initialize(id = id, param_set = ps, param_vals = param_vals, feature_types = c("numeric"))
    }
  ),

  private = list(
    .get_state = function(task) {
      # debug
      # pv = list(
      #   use = "everything",
      #   method = "pearson",
      #   cutoff = 0.9
      # )

      ########## THIS IS OLD WAY #############
      # cm = mlr3misc::invoke(stats::cor, x = data, use = pv$use, method = pv$method)
      # cm[upper.tri(cm)] <- 0
      # diag(cm) <- 0
      # cm <- abs(cm)
      # remove_cols <- colnames(data)[apply(cm, 2, function(x) any(x > pv$cutoff))]
      # keep_cols <- setdiff(fn, remove_cols)
      # list(cnames = keep_cols)
      ########## THIS IS OLD WAY #############

      fn = task$feature_types[type == self$feature_types, id]
      data = task$data(cols = fn)
      pv = self$param_set$values

      # Compute correlation matrix
      cor_mat = mlr3misc::invoke(
        stats::cor,
        x = data,
        use = pv$use,
        method = pv$method
      )
      cor_abs = abs(cor_mat)

      # Ignore upper triangle & diagonal for easier checking
      cor_abs[upper.tri(cor_abs)] = 0
      diag(cor_abs) = 0

      # We'll store indices of columns to remove
      to_remove = integer(0)

      # Walk through each column in sequence
      for (i in seq_len(ncol(cor_abs))) {
        # If this column is already marked for removal, skip
        if (i %in% to_remove) next

        # Find columns correlated above the threshold with column i
        high_cor_with_i = which(cor_abs[, i] > pv$cutoff)

        # Mark those columns (except i itself) for removal
        for (j in high_cor_with_i) {
          if (!(j %in% to_remove) && j != i) {
            to_remove = c(to_remove, j)
          }
        }
      }

      # Make sure we have unique indices only
      to_remove = unique(to_remove)

      # Prepare the final set of columns to KEEP
      keep_idx = setdiff(seq_len(ncol(data)), to_remove)
      keep_cols = colnames(data)[keep_idx]

      list(cnames = keep_cols)
    },

    .transform = function(task) {
      task$select(self$state$cnames)
    }
  )
)

#' @include zzz.R
register_po("dropcor", PipeOpDropCorr)

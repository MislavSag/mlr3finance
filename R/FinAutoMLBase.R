

# FinAutoMLBase = R6::R6Class(
#   "FinAutoMLBase",
#   public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @return [FinAutoMLBase][mlr3finance::FinAutoMLBase]
#     initialize = function(task,
#                           learners,
#                           inner_resampling,
#                           outer_resampling,
#                           measure,
#                           tuner,
#                           terminator) {
#       # Checks
#       assert_task(task)
#       assert_character(learner_list, any.missing = FALSE, min.len = 1)
#       for (learner in learner_list) {
#         assert_subset(learner, mlr_learners$keys())
#       }
#
#       # Set default values
#       self$task = task
#       self$learners = learners
#       self$inner_resampling = inner_resampling
#       self$outer_resampling = outer_resampling
#       self$measure = measure
#       self$tuner = tuner
#       self$terminator = terminator
#     },
#
#     # Define the method to run the AutoML process
#     run = function() {
#       # Implement the AutoML process here
#     }
#   ),
#
#   private = list(
#     task = NULL,
#     learners = NULL,
#     inner_resampling = NULL,
#     outer_resampling = NULL,
#     measure = NULL,
#     tuner = NULL,
#     terminator = NULL
#
# )

# library(data.table)
# library(gausscov)
# library(paradox)
# library(mlr3)
# library(mlr3pipelines)
# library(mlr3viz)
# library(mlr3tuning)
# library(mlr3misc)
# library(mlr3filters)
# library(future)
# library(future.apply)
# library(mlr3extralearners)
# library(batchtools)
# library(mlr3batchmark)
# library(checkmate)
# library(stringi)
# library(torch)
# library(mlr3torch)
# library(finautoml)
# library(lubridate)
# library(mlr3finance)
#
#
# # UTILS -------------------------------------------------------------------
# # utils https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates
# monnb <- function(d) {
#   lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
#   lt$year*12 + lt$mon }
# mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
#
#
# # PREPARE DATA ------------------------------------------------------------
# print("Prepare data")
#
# # read predictors
# if (interactive()) {
#   DT = fread("F:/strategies/H3/data_h3.csv")
# } else {
#   DT = fread("data_h3.csv")
# }
#
# # Checks
# DT[, .(date, date_rolling, yearmonthid)]
#
# # define predictors
# cols_non_features <- c("symbol", "date", "time", "right_time",
#                        "bmo_return", "amc_return", "date_transcripts",
#                        "date_dt",
#                        "open", "high", "low", "close", "volume", "returns",
#                        "yearmonthid", "date_rolling"
# )
# targets <- c(colnames(DT)[grep("ret_excess", colnames(DT))])
# cols_features <- setdiff(colnames(DT), c(cols_non_features, targets))
# "prob_neutral" %in% cols_features
#
# # convert columns to numeric. This is important only if we import existing features
# chr_to_num_cols <- setdiff(colnames(DT[, .SD, .SDcols = is.character]), c("symbol", "time", "right_time"))
# print(chr_to_num_cols)
# DT = DT[, (chr_to_num_cols) := lapply(.SD, as.numeric), .SDcols = chr_to_num_cols]
#
# # remove observations with missing target
# # if we want to keep as much data as possible an use only one predicitn horizont
# # we can skeep this step
# DT = na.omit(DT, cols = setdiff(targets, colnames(DT)[grep("extreme", colnames(DT))]))
#
# # filter data after 2013-07-01
# DT = DT[date >= as.IDate("2013-07-01")]
#
# # change IDate to date, because of error
# # Assertion on 'feature types' failed: Must be a subset of
# # {'logical','integer','numeric','character','factor','ordered','POSIXct'},
# # but has additional elements {'IDate'}.
# DT[, date := as.POSIXct(date, tz = "UTC")]
# # DT[, .(symbol,date, date_rolling, yearmonthid)]
#
# # sort
# # this returns error on HPC. Some problem with memory
# # setorder(DT, date)
# # DT = DT[order(date)] # DOESNT WORK TOO
# DT = DT[order(yearmonthid)]
#
# # Convert types
# DT = DT[, names(.SD) := lapply(.SD, as.numeric), .SDcols = bit64::is.integer64]
# colnames(DT)[grepl("date", colnames(DT))]
#
#
# # TASKS -------------------------------------------------------------------
# print("Tasks")
#
# # id coluns we always keep
# id_cols = c("symbol", "date", "yearmonthid")
#
# # task with future week returns as target
# target_ = colnames(DT)[grep("^ret.*xcess.*tand.*5", colnames(DT))]
# cols_ = c(id_cols, target_, cols_features)
# task_ret_week = as_task_regr(DT[, ..cols_], id = "taskRetWeek", target = target_)
#
# # task with future month returns as target
# target_ = colnames(DT)[grep("^ret.*xcess.*tand.*22", colnames(DT))]
# cols_ = c(id_cols, target_, cols_features)
# task_ret_month <- as_task_regr(DT[, ..cols_],
#                                id = "taskRetMonth",
#                                target = target_)
#
# # task with future 2 months returns as target
# target_ = colnames(DT)[grep("^ret.*xcess.*tand.*66", colnames(DT))]
# cols_ = c(id_cols, target_, cols_features)
# task_ret_quarter <- as_task_regr(DT[, ..cols_],
#                                  id = "taskRetQuarter",
#                                  target = target_)
#
# # set roles for symbol, date and yearmonth_id
# task_ret_week$set_col_roles("yearmonthid", "group")
# task_ret_week$col_roles$feature = setdiff(task_ret_week$col_roles$feature,
#                                           id_cols)
# task_ret_week$col_roles$group
# task_ret_month$set_col_roles("yearmonthid", "group")
# task_ret_month$col_roles$feature = setdiff(task_ret_month$col_roles$feature,
#                                            id_cols)
# task_ret_quarter$set_col_roles("yearmonthid", "group")
# task_ret_quarter$col_roles$feature = setdiff(task_ret_quarter$col_roles$feature,
#                                              id_cols)
#
#
# # cretate learners graph node
# learners_l = list(
#   ranger  = lrn("regr.ranger", id = "ranger"),
#   xgboost = lrn("regr.xgboost", id = "xgboost")
# )
#
# # create regression average of all learners
# choices = c("ranger", "xgboost")
# learners = po("branch", choices) %>>%
#   gunion(learners_l) %>>%
#   po("unbranch")
#
# graph = po("dropnacol", id = "dropnacol", cutoff = 0.05) %>>%
#   po("dropna", id = "dropna") %>>%
#   po("removeconstants", id = "removeconstants_1", ratio = 0)  %>>%
#   po("winsorize", id = "winsorize", lower = 0.01, upper = 0.99) %>>%
#   po("removeconstants", id = "removeconstants_2", ratio = 0)  %>>%
#   po("dropcor", id = "dropcorr", cutoff = 0.99) %>>%
#   po("uniform") %>>%
#   po("dropna", id = "dropna_v2") %>>%
#   learners
# plot(graph)
# graph_lrn = as_learner(graph)
#
# search_space_ = ps(
#   branch.selection = p_fct(choices)
# )
#
# # auto tuner rf
# at_graph = auto_tuner(
#   tuner = tnr("grid_search"),
#   learner = graph_lrn,
#   resampling = rsmp("holdout_gap_ratio", ratio = 0.7, gap = 1),
#   measure = msr("regr.mse"),
#   search_space = search_space_
# )
#
# design = benchmark_grid(
#   tasks = task_ret_week,
#   learners = at_graph,
#   resamplings = rsmp("gap_cv", initial_window = 119, horizon = 1, gap = 1, step = 1, rolling = FALSE)
# )
# design$resampling[[1]]$iters
#
# becnhmark = benchmark(design, store_models = TRUE)
#
# # Inspect results
# becnhmark$aggregate()
# extract_inner_tuning_results(becnhmark)
# extract_inner_tuning_archives(becnhmark)
# becnhmark$resamplings$resampling[[1]]$iters
# becnhmark$resamplings$resampling[[1]]$train_set(1)
# becnhmark$resamplings$resampling[[1]]$train_set(1)[1]
# tail(becnhmark$resamplings$resampling[[1]]$train_set(1), 1)
# becnhmark$resamplings$resampling[[1]]$test_set(1)[1]
# tail(becnhmark$resamplings$resampling[[1]]$test_set(1), 1)
# becnhmark$resamplings$resampling[[1]]$train_set(2)[1]
# tail(becnhmark$resamplings$resampling[[1]]$train_set(2), 1)
#
# becnhmark$resamplings$resampling[[1]]$instance
# as.Date(becnhmark$resamplings$resampling[[1]]$instance$train[[1]])
# as.Date(becnhmark$resamplings$resampling[[1]]$instance$test[[1]])
# as.Date(becnhmark$resamplings$resampling[[1]]$instance$train[[2]])
# as.Date(becnhmark$resamplings$resampling[[1]]$instance$test[[2]])
#
# x = becnhmark$resample_result(1)
# x$resampling$train_set(1)
# x$resampling$train_set(1)[1]
# tail(x$resampling$train_set(1), 1)
# tail(becnhmark$resamplings$resampling[[1]]$train_set(1), 1)
#
# x = extract_inner_tuning_archives(becnhmark)
# x$resample_result[[1]]
# x$resample_result[[1]]$resampling$train_set(1)[1]
# tail(x$resample_result[[1]]$resampling$train_set(1), 1)
# x$resample_result[[1]]$resampling$test_set(1)[1]
# tail(x$resample_result[[1]]$resampling$test_set(1), 1)
# tail(becnhmark$resamplings$resampling[[1]]$train_set(1), 1)
#
# x$resample_result[[3]]$resampling$train_set(1)[1]
# tail(x$resample_result[[3]]$resampling$train_set(1), 1)
# x$resample_result[[3]]$resampling$test_set(1)[1]
# tail(x$resample_result[[3]]$resampling$test_set(1), 1)
# tail(becnhmark$resamplings$resampling[[1]]$train_set(2), 1)

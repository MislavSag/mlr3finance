#' @import data.table
#' @import mlr3misc
#' @import checkmate
#' @import paradox
#' @import mlr3
#' @import R6
#' @importFrom utils globalVariables packageVersion

utils::globalVariables(c(
  "row_id", "resampling", "task", "fold", "id", "type", "test", "N"))

mlr3finance_pipeops = new.env()
mlr3finance_pipeop_tags = "finance"

# metainf must be manually added in the register_mlr3pipelines function
# Because the value is substituted, we cannot pass it through this function
register_po = function(name, constructor) {
  if (utils::hasName(mlr3finance_pipeops, name)) stopf("pipeop %s registered twice", name)
  mlr3finance_pipeops[[name]] = constructor
}

register_mlr3 = function() {

  # resampling methods ---------------------------------------------------------

  mlr_resamplings = utils::getFromNamespace("mlr_resamplings", ns = "mlr3")
  mlr_resamplings$add("gap_cv", ResamplingGapCV)
  mlr_resamplings$add("holdout_gap_ratio", ResamplingHoldoutGapRatio)
  mlr_resamplings$add("holdout_gap_fix", ResamplingHoldoutGapFix)
}

register_mlr3pipelines = function() {
  mlr_reflections = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  mlr_pipeops = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  iwalk(as.list(mlr3finance_pipeops), function(value, name) {
    # print(value)
    # print(name)
    # value = as.list(mlr3fin_pipeops)[[1]]
    # mlr_pipeops$add(name, value$constructor, value$metainf)
    mlr_pipeops$add(name, value)
  })
  mlr_reflections$pipeops$valid_tags = union(mlr_reflections$pipeops$valid_tags,
                                             mlr3finance_pipeop_tags)
}

.onLoad = function(libname, pkgname) { # nolint
  # print(pkgname)
  register_mlr3()
  assign("lg", lgr::get_logger("mlr3"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
  register_namespace_callback(pkgname, "mlr3", register_mlr3)
  register_namespace_callback(pkgname, "mlr3pipelines", register_mlr3pipelines)
}

.onUnload = function(libpath) { # nolint
  # event = packageEvent("mlr3", "onLoad")
  # hooks = getHook(event)
  # pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  # setHook(event, hooks[pkgname != "finance"], action = "replace")

  mlr_resamplings = mlr3::mlr_resamplings
  walk(names(resamplings), function(id) mlr_resamplings$remove(id))
}

leanify_package() # nocov end

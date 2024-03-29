check_has_name <- function(name) {
  if (missing(name)) {
    rlang::abort(message = c("`name` must be supplied."))
  }
}

check_if_flow_and_custom_dims <- function(flow, nrow, ncol) {
  if (flow == "row" & (length(nrow) > 1)) {
    rlang::abort(message = c("length(`nrow`) > 1, but `flow` == \"row\"",
                             i = "nrow must be constant if you wish to wrap along rows",
                             i = "(ncol can vary, though)"))
  }

  if (flow == "col" & (length(ncol) > 1)) {
    rlang::abort(message = c("length(`ncol`) > 1, but `flow` == \"col\"",
                             i = "ncol must be constant if you wish to wrap along columns",
                             i = "(nrow can vary, though)"))
  }
}



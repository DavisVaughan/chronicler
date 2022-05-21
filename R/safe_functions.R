#' Filter a data frame using predicates
#' @details
#' This function is nothing but a wrapper around `dplyr::filter()`, but raises a warning
#' if the output is an empty data frame.
#' @param ... Further arguments passed down to `dplyr::filter()`.
#' @importFrom dplyr filter
#' @return The filtered data frame
#' @export
filter2 <- function(...){

  result <- dplyr::filter(...)

  if(nrow(result) == 0){
    rlang::warn(message = "Filter resulted in an empty df. Check predicates.")
  }

  result
}

#' Select a data frame using predicates
#' @details
#' This function is nothing but a wrapper around `dplyr::select()`, but raises a warning
#' if the output is an empty data frame.
#' @param ... Further arguments passed down to `dplyr::select()`.
#' @importFrom dplyr select
#' @return The selected data frame
#' @export
select2 <- function(...){

  result <- dplyr::select(...)

  if(ncol(result) == 0){
    rlang::warn(message = "Select resulted in an empty df. Check predicates.")
  }

  result
}

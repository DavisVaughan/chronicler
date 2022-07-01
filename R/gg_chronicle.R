#' @export
ggpurely <- function(.f, strict = 2){

  function(..., .log_df = "Log start..."){

    if(maybe::is_nothing(...)){

      final_result <- list(
        value = maybe::nothing(),
        log_df = "A `Nothing` was given as input."
      )

    } else {
      res <- switch(strict,
                    only_errors(.f, ...),
                    errors_and_warnings(.f, ...),
                    errs_warn_mess(.f, ...))

      final_result <- list(
        value = NULL,
        log_df = NULL
      )

      final_result$value <- if(any(c("error", "warning", "message") %in% class(res))){
                              maybe::nothing()
                            } else {
                              maybe::just(res)
                            }

      final_result$log_df <- if(any(c("error", "warning", "message") %in% class(res))){
                               rlang::cnd_message(res)
                             } else {
                               NA
                             }
      }

    final_result
    }

}

#' @export
ggrecord <- function(.f, .g = (\(x) NA), strict = 2){

  fstring <- deparse1(substitute(.f))

  function(...){

    args <- paste0(rlang::enexprs(...), collapse = ",")

    start <- Sys.time()
    pure_f <- ggpurely(.f, strict = strict)
    res_pure <- (pure_f(...))
    end <- Sys.time()

    if(maybe::is_nothing(res_pure$value)){

      log_df <- make_log_df(
        success = 0,
        fstring = fstring,
        args = args,
        res_pure = res_pure,
        start = start,
        end = end,
        .g = .g
      )

    } else {

      log_df <- make_log_df(
        success = 1,
        fstring = fstring,
        args = args,
        res_pure = res_pure,
        start = start,
        end = end,
        .g = .g,
        diff_obj = NULL,
      )

    }

    log_df <- dplyr::mutate(
                             log_df,
                       ops_number = dplyr::row_number(),
                       lag_outcome = dplyr::lag(outcome, 1)
                     )

    if(maybe::is_nothing(res_pure$value)
       & tail(log_df, 1)$ops_number == 1){
      log_df$message <- paste0(res_pure$log_df, collapse = " ")
    } else if (maybe::is_nothing(res_pure$value)
               & !grepl("Success", tail(log_df, 1)$lag_outcome)
               & tail(log_df, 1)$ops_number > 1){
      log_df[nrow(log_df), ]$message <- "Pipeline failed upstream"
    }

    list_result <- list(
      value = res_pure$value,
      log_df = log_df
    )

    structure(list_result, class = "chronicle")

  }

}

maybe_ggadd <- function(e1, e2){

  gg1 <- maybe::from_maybe(e1$value, default = maybe::nothing())
  gg2 <- maybe::from_maybe(e2$value, default = maybe::nothing())

  if(maybe::is_nothing(gg1) | maybe::is_nothing(gg2)){
    return(maybe::nothing())
  } else {
    maybe::maybe(ggplot2::`%+%`)(gg1, gg2)
  }
}

#' @export
`%>+%` <- function(e1, e2){

  structure(
    list("value" =  maybe_ggadd(e1, e2),
         "log_df" = rbind(e1$log_df,
                       e2$log_df)),
    class = "ggchronicle"
  )

}

#' @export
document_gg <- function(.c, overwrite_caption = TRUE){

  r_labs <- ggrecord(labs)

  .c_caption <- (maybe::from_maybe(.c$value,
                                  default = maybe::nothing()))$labels$caption

  cap <- if(overwrite_caption){
           paste0(read.log(.c), collapse = "\n")
         } else {

           paste0(c(.c_caption,  read.log(.c)), collapse = "\n")
           }

  .c %>+%
    r_labs(caption = cap)


}

#' @export
document_gg2 <- function(.c, layout = "
    AAAA
    AAAA
    BBBB
    "
  ){

  .c2 <- ggplot() +
    annotate("text", x = .5, y = .5, label = paste0(read.log(.c), collapse = "\n")) +
    theme_void()

  patchwork:::`&.gg`(
                patchwork:::`/.ggplot`(pick(a, "value"), .c2),
                plot_layout(design = layout))

}


ggadd <- function(e1, e2, strict = 1){

  switch(strict,
         only_errors(`+`, e1, e2),
         errors_and_warnings(`+`, e1, e2),
         errs_warn_mess(`+`, e1, e2))


}


maybe_ggadd <- function(e1, e2){

  gg1 <- maybe::from_maybe(e1$value, default = maybe::nothing())
  gg2 <- maybe::from_maybe(e2$value, default = maybe::nothing())

  if(maybe::is_nothing(gg1) | maybe::is_nothing(gg2)){
    return(maybe::nothing())
  } else {
    maybe::maybe(ggplot2::`%+%`)(gg1, gg2)
  }
}

#' @export
`%>+%` <- function(e1, e2){

  structure(
    list("value" =  maybe_ggadd(e1, e2)$value,
         "log_df" = rbind(e1$log_df,
                          e2$log_df)),
    class = "chronicle"
  )

}

ggadd <- function(e1, e2, strict = 1, .log_df = "Starting ggplot..."){


  intermediary_result <- list(
    value = NULL,
    log_df = NULL
    )

  # when using ggplot, errors can only get
  # captured at printing time
  gg <- e1$value + e2$value

  res <- switch(strict,
                only_errors(print, gg),
                errors_and_warnings(print, gg),
                errs_warn_mess(print, gg))


  intermediary_result$value <- if(any(c("error", "warning", "message") %in% class(res))){
                          maybe::nothing()
                        } else {
                          maybe::just(res)
                        }

  intermediary_result$log_df <- if(any(c("error", "warning", "message") %in% class(res))){
                           rlang::cnd_message(res)
                         } else {
                           NA
                         }

  structure(
    list("value" =  intermediary_result$value,
         "log_df" = rbind(e1$log_df,
                          e2$log_df)),
    class = "chronicle"
  )
  

  intermediary_result


}

#ggpurely2 <- function(.f, strict = 2){
#
  #function(..., .log_df = "Log start..."){
#
    #if(maybe::is_nothing(...)){
#
      #final_result <- list(
        #value = maybe::nothing(),
        #log_df = "A `Nothing` was given as input."
      #)
#
    #} else {
      #res <- switch(strict,
                    #only_errors(.f, ...),
                    #errors_and_warnings(.f, ...),
                    #errs_warn_mess(.f, ...))
#
      #final_result <- list(
        #value = NULL,
        #log_df = NULL
      #)
#
      #final_result$value <- if(any(c("error", "warning", "message") %in% class(res))){
                              #maybe::nothing()
                            #} else {
                              #maybe::just(res)
                            #}
#
      #final_result$log_df <- if(any(c("error", "warning", "message") %in% class(res))){
                               #rlang::cnd_message(res)
                             #} else {
                               #NA
                             #}
      #}
#
    #final_result
    #}
#
#}

make_log_df2 <- function(ops_number = 1,
                        fstring,
                        args){


  tibble::tibble(
            "ops_number" = ops_number,
            "function" = fstring,
            "arguments" = args,
            #"message" = paste0(res_pure$log_df, collapse = " "),
            "lag_outcome" = NA
          )

}

ggrecord2 <- function(.f){

  fstring <- deparse1(substitute(.f))

  function(...){

    args <- paste0(rlang::enexprs(...), collapse = ",")

    res_pure <- .f(...)


    log_df <- make_log_df2(
      fstring = fstring,
      args = args)

    log_df <- dplyr::mutate(
                             log_df,
                       ops_number = dplyr::row_number())

  #  list_result <- list(
  list_result <- list(
      value = res_pure,
      log_df = log_df
    )

    structure(list_result, class = "ggchronicle")

  }

}


r_ggplot <- ggrecord2(ggplot)
r_geom_point <- ggrecord2(geom_point)
r_labs <- ggrecord2(labs)

ggadd(r_ggplot(mtcars), r_geom_point(aes(y = hp, x = mp)))

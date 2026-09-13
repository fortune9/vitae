#' Create onecoltable entries
#'
#' @param data A `data.frame` or `tibble` containing the onecoltable text entries
#' @param details  the column name in `data` that contains the details
#' for each entry
#' @param .protect Logical, if TRUE (default) protects special LaTeX characters in input
#'
#' @return Object of class \code{vitae_onecoltable}
#' @importFrom rlang enexpr expr_text !!
#' @export
onecoltable_entries <- function(data, details, .protect = TRUE) {
  onecoltable_exprs <- list(
    details = enquo(details) %missing% NA_character_
  )

  out <- dplyr::as_tibble(map(onecoltable_exprs, eval_tidy, data = data))
  structure(out,
    preserve = names(onecoltable_exprs),
    protect = .protect,
    class = c("vitae_onecoltable", "vitae_preserve", class(data))
  )
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.vitae_onecoltable <- function(x) {
  x <- NextMethod()
  c(x, "vitae type" = "onecoltable entries")
}

#' @importFrom knitr knit_print
#' @export
knit_print.vitae_onecoltable <- function(x, options, ...) {
  if(is.null(entry_format_functions$format)) {
    warn("Plain entry formatter is not defined for this output format.")
    return(knit_print(tibble::as_tibble(x)))
  }

  format <- entry_format_functions$format$onecoltable
  if (is.null(format)) {
    warning("Plain format not supported for this template, using brief format instead")
    format <- entry_format_functions$format$brief
  }
  
  x[is.na(x)] <- ""

  if(!(x%@%"protect")){
    protect_tex_input <- identity
  }

  knitr::asis_output(
    format(
      protect_tex_input(x$details)
    )
  )
}

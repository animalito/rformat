#' Converts number to string with pretty format.
#'
#' @param n Integer or float.
#'
#' @examples
#' pnumber.s(459877364)
#' pnumber.s(34556.45778)
pnumber.s <- function(n){
  if((n - floor(n)) > 0){
    nf <- format(round(n, digits=2), big.mark=",", preserve.width="none")
  } else {
    nf <- format(floor(n), big.mark=",", preserve.width="none")
  }
  return(nf)
}

#' Converts number or numeric to string or character with pretty format.
#'
#' @param number Numeric, integer or number.
#'
#' @return character vector with readable format for numbers.
#'
#' @examples
#' pnumber(454667)
#' pnumber(456234.3434356)
#' pnumber(c(453535635, 364634.6245624, 43634.35235236, 32535234523))
#' @export
#' @importFrom stringr "str_trim"
pnumber <- function(number){
  if (length(number) > 1) {
    fn <- sapply(number, FUN = function(x) {pnumber.s(x)})
  } else {
    fn <- pnumber.s(number)
  }
  fn <- stringr::str_trim(fn)
  return(fn)
}


#' Formats a data frame numeric and integer columns into a readable format.
#'
#' @param data.frame df
#' @param fnames By default, \code{fnames = TRUE} transforms the names of the
#'   columns into readable names using fancy_names. To leave them identical use
#'   \code{fnames = FALSE}.
#'
#' @examples
#' options(scipen = 999, digits = 10) # disable scientific notation
#' df <- data.frame(
#'   big_numbers = rnorm(10, 1000000,1000000),
#'   big_integers = floor(rnorm(10, 1000000,1000000)),
#'   mixed.numbers = c(rnorm(5, 100, 50), rnorm(5, 100, 50))
#' )
#' df
#' ptable(df)
#' @export
ptable <- function(df, fnames = TRUE){
  df <- data.frame(df)
  # Numeros y enteros los hacemos leibles y bonitos
  clases <- sapply(df, class)
  for(i in c(1:length(clases))){
    if(clases[i] %in% c("numeric", "integer")) {
      df[, i] <- pnumber(df[, i])
    }
  }
  if (fnames == TRUE) {
    names(df) <- fancy_names(names(df))
  }
  return(df)
}

#' Get totals and percentages.
#'
#' @param \dots Additional arguments.
#' @param percentages data.frame with percentages.
#' @param totals data.frame with totals.
#'
#' @examples
#' library(tidyr)
#' totals <- data.frame(names = letters[1:5], x = rnorm(5, 500, 200), y = rnorm(5, 500, 200))
#' per <- tidyr::gather(totals, key = key, value = value, -names) %>% dplyr::group_by(key) %>% dplyr::mutate(percentage = value/sum(value) * 100) %>% dplyr::select(-value) %>% tidyr::spread(key = key, value = percentage) %>% data.frame()
#' total_percentage(totals, per)
#' total_percentage(totals, per, fnames = F)
#' @export
total_percentage <- function(totals, percentages, ...){
  if ( nrow(totals) != nrow(percentages) ) {
    stop("The tables do not have the same number of rows.")
  }
  if ( nrow(totals) != nrow(percentages) ) {
    stop("The tables do not have the same number of columns.")
  }
  if ( !identical(totals[, 1], percentages[, 1]) ) {
    stop("Tables do not have identical dimensions.")
  }
  tab.tot <- ptable(totals, ...)
  tab.por <- ptable(percentages, ...)
  for(i in c(2:ncol(tab.tot))){
    tab.tot[, i] <- paste0(tab.tot[, i], " (", tab.por[, i], "%)")
  }
  return(tab.tot)
}

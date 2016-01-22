#' Converts number to string with pretty format.
#'
#' @param n Integer or float.
#'
#' @examples
#' fnumber.s(459877364)
#' fnumber.s(34556.45778)
fnumber.s <- function(n){
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
#' fnumber(454667)
#' fnumber(456234.3434356)
#' fnumber(c(453535635, 364634.6245624, 43634.35235236, 32535234523))

#' @importFrom stringr "str_trim"
fnumber <- function(number){
  if (length(number) > 1) {
    fn <- sapply(number, FUN = function(x) {fnumber.s(x)})
  } else {
    fn <- fnumber.s(number)
  }
  fn <- stringr::str_trim(fn)
  return(fn)
}

ftable <- function(df){
  df <- data.frame(df)
  # Numeros y enteros los hacemos leibles y bonitos
  clases <- sapply(df, class)
  for(i in c(1:length(clases))){
    if(clases[i] %in% c("numeric", "integer")) {
      df[, i] <- fnumber(df[, i])
    }
  }

  # Ahora, arreglamos los nombres
  #names(df) <- nombresMaricas(names(df))
  return(df)
}

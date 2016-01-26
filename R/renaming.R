#' \code{normalize_names} returns a character vector with sql names conformant.
#'
#' Removes trailing spaces, changes multiple spaces for one, changes spaces and
#' "." for "_", transforms camelCase to camel_case, removes Spanish characters
#' -accents and ñ-, transforms all to lowercase.
#'
#' @param A character vector. Column names with invalid characters.
#'
#' @return A character vector.
#'
#' @examples
#' normalize_names(c("Hello  world   ", "another.name", "camelCase", "ALL"))
#' @export
#' @importFrom magrittr "%>%"
normalize_names <- function(column_names) {
  gsub("^ *|(?<= ) | *$", "", column_names, perl=T) %>%
    gsub('\\ |\\.', '_', .) %>%
    gsub("([a-z])([A-Z])", "\\1_\\L\\2", ., perl = TRUE) %>%
    gsub('ñ', 'n', .) %>%
    iconv(., to='ASCII//TRANSLIT') %>%
    tolower(.)
}

#' \code{fancy_names} returns a character vector with pretty names for tables.
#'
#' Changes what should be space to it (-, _, and .). First letter in word
#' to uppercase.
#'
#' @param A character vector. Normalized names.
#'
#' @return A character vector.
#'
#' @examples
#' fancy_names(c("a.good_name", "another-name"))
#' @export
fancy_names <- function(x){
  gsub("(\\_|\\.|\\-)([A-Za-z])", " \\U\\2", x, perl = T) %>%
    gsub("^([A-Za-z])", "\\U\\1", ., perl = T)
}

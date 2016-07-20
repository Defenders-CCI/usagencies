# BSD_2_clause

#' Get agencies matching a query string
#'
#' Get agencies matching a query string using approximate pattern matching
#' (agrep).
#'
#' @param query Query string for the branch/department/agency of interest
#' @return A data.frame of rows matching the query
#' @export
#' @examples
#' get_agencies("Fish and Wildlife")
get_agencies <- function(query) {
  mat <- usagencies[agrep(usagencies$combo, pattern = query), 1:5]
  return(mat)
}

#' List of all branches, departments, agencies, etc.
#'
#' @return A vector of all entities in usagencies
#' @export
#' @examples
#' list_usgov()
list_usgov <- function() {
  unlisted <- unique(unlist(as.list(usagencies[, 1:5])))
  return(unlisted)
}

#' Clean binary variables
#'
#' @description This function takes a variable coded "Yes"/"No" and converts it to either
#' a logical variable (the default, where "Yes" is converted to TRUE) or to a factor.
#'
#' @useage esp_binary(.data)
#'
#' @param .data A data frame or tibble
#' @param currentVar The current source variable
#' @param newVar The name of a new variable to be created
#' @param logical A logical scalar; should a logical variable be created? If \code{FALSE}, a factor is created.
#'
#' @return A data frame or a tibble with the newly created variable.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @export
esp_binary <- function(.data, currentVar, newVar, logical = TRUE){

  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(newVar)) {
    stop('A new variable name must be specified for newVar')
  }

  if (missing(currentVar)) {
    stop('An existing variable must be specified for currentVar')
  }

  # reformat currVar input
  if (!is.character(paramList$currentVar)) {
    currVar <- rlang::enquo(currentVar)
  } else if (is.character(paramList$currentVar)) {
    currVar <- rlang::quo(!! rlang::sym(currentVar))
  }

  # reformat newVar input
  if (!is.character(paramList$newVar)) {
    createVar <- rlang::enquo(newVar)
  } else if (is.character(paramList$newVar)) {
    createVar <- rlang::quo(!! rlang::sym(newVar))
  }

  createVarQ <- rlang::quo_name(rlang::enquo(newVar))

  # recode variable
  if (logical == TRUE){

    .data <- dplyr::mutate(.data, !!createVarQ := ifelse(!!currVar == "Yes", TRUE, FALSE))

  }
  else if (logical == FALSE){

    .data <- dplyr::mutate(.data, !!createVarQ := as.factor(!!currVar))

  }

  # return data object
  return(.data)

}

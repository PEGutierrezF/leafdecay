#' @title Leaf Litter Decomposition Analysis for Stream Ecosystems
#'
#' @description Calculate mass loss by transport to the field and manipulation
#'
#' @param data
#'
#' @param InitialWt
#'
#' @param FinalWt
#'
#' @param Treatment
#'
#' @return meanControl
#'
#' @examples manipulation(data,InitialWt,FinalWt,Treatment)
#'
#' @export manipulation
manipulation <- function(data,InitialWt,FinalWt,Treatment,...) {
  control <- data %>%
    dplyr::filter(Treatment == "Control") %>%
    dplyr::select({{InitialWt}},{{FinalWt}}) %>%
    dplyr::mutate(Difference = {{FinalWt}}/{{InitialWt}})

  . <- mean(control$Difference, na.rm = TRUE)
  meanControl <- .*100
  return (meanControl)
}

#' @title Leaf Litter Decomposition Analysis for Stream Ecosystems
#'
#' @description Calculate slope
#'
#' @param data
#'
#' @param Treatment
#'
#' @param Replicate
#'
#' @param Day
#'
#' @param Ln.AFDMrem
#'
#' @return NULL
#'
#' @examples slope.K <- function(data,Treatment,Replicate,Day,Ln.AFDMrem)
#'
#' @export slope
slope.k <- function(data,
Treatment,
Replicate,
Day,
Ln.AFDMrem){
  fitted_models <- data  %>% group_by(Treatment, Replicate) %>%
    do(model = lm(Ln.AFDMrem ~ Day, data = .))

  broom::tidy(fitted_models,model) %>% print(n = Inf) # Calculate the slope and estimate

}

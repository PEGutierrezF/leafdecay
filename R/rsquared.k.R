#' @title Leaf Litter Decomposition Analysis for Stream Ecosystems
#'
#' @description Calculate r-squared
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
#' @examples rsquared.k(data,Treatment,Replicate,Day,Ln.AFDMrem)
#'
#' @export rsquared.k
rsquared.k <- function(data,
Treatment,
Replicate,
Day,
Ln.AFDMrem){
  fitted_models <- data  %>% group_by(Treatment, Replicate) %>%
    do(model = lm(Ln.AFDMrem ~ Day, data = .))

  broom::glance(fitted_models,model) %>% print(n = Inf) # Calculate the r-squared and p-value

}

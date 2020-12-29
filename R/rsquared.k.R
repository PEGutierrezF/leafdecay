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
rsquared.k<- function(data, Treatment, Replicate, Day, Ln.AFDMrem){
  ln_col <- rlang::as_string(ensym(Ln.AFDMrem))
  day_col <- rlang::as_string(ensym(Day))
  data %>%
    nest_by({{Treatment}}, {{Replicate}}) %>%
    mutate(model = list(lm(reformulate(day_col, ln_col), data = data))) %>%
    summarise(glance_out = list(glance(model)), .groups = 'drop') %>%
    unnest(glance_out)%>% print(n = Inf) # Calculate the r-squared and p-value
}

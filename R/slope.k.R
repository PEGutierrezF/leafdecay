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
#' @examples slope.K(data,Treatment,Replicate,Day,Ln.AFDMrem)
#'
#' @export slope
slope.k <- function(data, Treatment, Replicate, Day, Ln.AFDMrem){
  ln_col <- rlang::as_string(ensym(Ln.AFDMrem))
  day_col <- rlang::as_string(ensym(Day))
  data %>%
    nest_by({{Treatment}}, {{Replicate}}) %>%
    mutate(model = list(lm(reformulate(day_col, ln_col), data = data))) %>%
    summarise(tidy_out = list(tidy(model)), .groups = 'drop') %>%
    unnest(tidy_out)
}

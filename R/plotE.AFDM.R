#' @title Leaf Litter Decomposition Analysis for Stream Ecosystems
#'
#' @description Made a variety of Plots to explore the data
#'
#' @param data
#'
#' @param Treatment
#'
#' @param Replicate
#'
#' @param Day
#'
#' @param AFDMrem
#'
#' @return NULL
#'
#' @examples plot.E(data)
#'
#' @export plot.E
plot.E <- function(data)
{
  ggplot(data, (aes(x = Day, y = AFDMrem))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Treatment + Replicate)
}

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
#' @examples plot.B(data)
#'
#' @export plot.B
plot.B <- function(data)
{
  ggplot(data,(aes(x = Day, y = AFDMrem, colour = factor(Replicate)))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # Splitting the single figure into multiple depending on treatment
    facet_wrap(~ Treatment)
}

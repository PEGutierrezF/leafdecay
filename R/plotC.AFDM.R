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
#' @examples plot.C(data)
#'
#' @export plot.C
plot.C <- function(data)
{
  ggplot(data, aes(x = Day, y = AFDMrem)) +
    geom_point() +
    geom_smooth(aes(colour=Treatment ), method = "lm", se = FALSE)
}

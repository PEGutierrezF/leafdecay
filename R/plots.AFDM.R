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
#' @examples plot.A(data)
#'
#' @export plot.A
plot.A <- function(data)
{
  data%>% # the names of the new data frame and the data frame to be summarised
    group_by(Treatment, Day) %>%   # the grouping variable
    dplyr::summarise(mean = mean(AFDMrem),  # calculates the mean of each group
                     sd = sd(AFDMrem), # calculates the standard deviation of each group
                     n = n(),  # calculates the sample size per group
                     SE = sd(AFDMrem)/sqrt(n()) # calculates the standard error of each group
    ) %>%
    ggplot(aes(x = Day , y= mean, group = Treatment,color=Treatment))+
    geom_line(size=1)+ geom_point(size=2)  +
    #    geom_smooth(method = "lm", se=FALSE, color="blue", formula = y ~ x)+
    geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), width=0.5) +
    xlab('Day') + ylab('AFDM remaining') +
    facet_wrap(~ Treatment)
}
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
#'
#' @examples plot.D(data)
#'
#' @export plot.D
plot.D <- function(data)
{
  ggplot(data,(aes(x = Day, y = AFDMrem, colour = Treatment))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # Splitting the single figure into multiple depending on treatment
    facet_wrap(~ Replicate)
}
#'
#' @examples plot.E(data)
#'
#' @export plot.E
plot.E <- function(data)
{
  ggplot(data, (aes(x = Day, y = AFDMrem, colour = Replicate))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Treatment + Replicate)
}

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

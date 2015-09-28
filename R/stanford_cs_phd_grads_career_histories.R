#' Returns a plot of fraction of career trajectories. 
#' 
#' Returns a ggplot2 plot of Stanford CS PhD choices over time. 
#' 
#' @return String 
#' @export
   
StanfordCSPhDGradsCareerHistories <- function(){
    data('df.p1')

    ggplot(data = subset(df.p1, type %in% c("Engineer", "Founder"))) + geom_rect(aes(
               xmin = StartYear,
               xmax = EndYear,
               ymin = rank.order,
               ymax = rank.order + 1,
               fill = type)) +
                   theme_bw() +
                       facet_grid(any.founder ~ any.engineer)   
}

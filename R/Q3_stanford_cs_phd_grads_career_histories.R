#' Returns a plot of fraction of career trajectories. 
#' 
#' Returns a ggplot2 plot of Stanford CS PhD choices over time. 
#' 
#' @return ggplot object
#' @export
#' 
StanfordCSPhDGradsCareerHistories <- function() {
  data('df_3')
  
  # Labeller for facets
  custom_labeller <- function(variable, value) {
    if (variable == "any.founder") {
      ifelse(value, "Founder", "Not Founder")
    } else {
      ifelse(value, "Engineer", "Not Engineer")
    }
  }
  
  ggplot(data = subset(df_3, type %in% c("Engineer", "Founder"))) +
    geom_rect(aes(xmin = StartYear,
                  xmax = EndYear,
                  ymin = rank.order,
                  ymax = rank.order + 1, fill = type)) +
    facet_grid(any.founder ~ any.engineer, labeller = custom_labeller) +
    theme_bw() +
    theme(legend.title = element_blank())
}

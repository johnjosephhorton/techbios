#' Returns a plot of timeline for a single graduate
#' 
#' Returns a ggplot2 plot of Stanford CS PhD timeline
#' 
#' @param id graduate ID
#' @param grads_only logical, if only positions after graduation should be plotted
#' @return ggplot object
#' @export
#' 
StanfordCSPhDGradsTimeline <- function(id, grads_only = FALSE) {
  
  # Loads data
  if (grads_only) {
    data('df_4_limited')
    df_4 <- df_4_limited
  } else {
    data('df_4')
  }
  
  # Filters data for given candidate
  plot_data <- df_4 %>% filter(PersonID == id)
  
  if (nrow(plot_data) < 1) {
    stop(sprintf("Can't find graduate with ID = '%s'", id))
  }
  
  # Extracts full name of the person
  full_name <- plot_data$FullName[1]
  
  ggplot(data = plot_data) +
    geom_rect(aes(xmin = StartYear, xmax = EndYear,
                  ymin = ExperienceID, ymax = ExperienceID + 1), fill = "gray", alpha = 0.5) +
    geom_text(aes(x = (StartYear + EndYear)/2, y = ExperienceID + 3/4, label = CompanyName)) +
    geom_text(aes(x = (StartYear + EndYear)/2, y = ExperienceID + 1/4, label = Title)) +
    theme_bw() + 
    theme(axis.text.y = element_blank()) + 
    xlab("Year") + 
    ylab("") + 
    ggtitle(full_name)
}

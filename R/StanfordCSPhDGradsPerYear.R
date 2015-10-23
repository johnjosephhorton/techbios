#' Returns a plot of the count of Stanford CS PhD graduates per year. 
#' 
#' Returns a ggplot2 plot of Stanford CS PhD graduates per year. 
#' 
#' @return ggplot object
#' @export
#' 
StanfordCSPhDGradsPerYear <- function() {
  data('GRADS')

  df_1 <- GRADS %>%
      group_by(PersonId, Name, Surname) %>%
      dplyr::summarise(StartYear = min(StartYear, na.rm = TRUE),
                EndYear = max(EndYear, na.rm = TRUE)) %>%
      ungroup() %>%
      na.omit() %>%
      group_by(EndYear) %>%
      dplyr::summarise(NumGrads = n())

  
  g <- ggplot(data = df_1, aes(x = EndYear, y = NumGrads)) +
         geom_line() +
         theme_bw() +
         xlab("Year of graduation from Stanford") +
         ylab("Number of graduates")

  g
}

#' Number of startups per cohort
#'
#' Number of startups per cohort
#'
#' @return ggplot2
#' @export

StartupsPerCohort <- function(){
    data("yctc_startups")

    startups <- yctc_startups
    
    df.1 <- startups %>%
        filter(src == "yc") %>%
            group_by(cohort) %>%
                summarise(startups = n()) %>%
                    ungroup() 

    g.per.yc.cohort <-
        ggplot(data = df.1) +
            geom_line(aes(x = 1:nrow(df.1), y = startups)) +
                scale_x_continuous(breaks = 1:nrow(df.1), labels = df.1$cohort) +
                    theme_bw() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                            xlab("Cohort") + ylab("Number of Start-Ups")
    g.per.yc.cohort
}

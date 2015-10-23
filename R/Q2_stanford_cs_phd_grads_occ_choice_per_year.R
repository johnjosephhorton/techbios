#' Returns a plot of fraction of Stanford CS PhD grads choosing various career options over time
#' 
#' Returns a ggplot2 plot of Stanford CS PhD grad occupational choices
#' 
#' @return ggplot object 
#' @export
#' 

StanfordCSPhDGradsOccChoicePerYear <- function() {
    data("GRADS")
    data("experience")
    data("id_list")
    
    founders <- GRADS %>%
        select(PersonId, GraduationYear = EndYear) %>%
        left_join(experience, by = c("PersonId" = "PersonID")) %>%
        mutate(
         is_founder = TitleID %in% id_list$founder_id & StartYear >= GraduationYear,
         is_engineer = TitleID %in% id_list$engineer_id & StartYear >= GraduationYear,
            years_after_graduation = as.numeric(StartYear) - as.numeric(GraduationYear)
        ) %>%
       group_by(PersonId) %>%
       do({
           was_founder <- any(.$is_founder, na.rm = TRUE)
           was_engineer <- any(.$is_engineer, na.rm = TRUE)
           if (was_founder) {
               year_founder <- filter(., is_founder) %$% min(StartYear, na.rm = TRUE)
           } else {
               year_founder <- NA
           }
           if (was_engineer) {
               year_engineer <- filter(., is_engineer) %$% min(StartYear, na.rm = TRUE)
           } else {
               year_engineer <- NA
           }
           data_frame(
               graduation_year = first(.$GraduationYear),
               was_founder = was_founder,
               year_founder = year_founder,
               was_engineer = was_engineer,
               year_engineer = year_engineer
           )
       }) %>%
       ungroup()

                                        # Calculates number of founders after graduation
    years <- range(founders$graduation_year, na.rm = TRUE)
    df_2 <- data_frame(year = years[1]:years[2]) %>%
        rowwise() %>%
            do({
                .year <- .$year
                founders %>%
                filter(graduation_year <= .year) %>%
                mutate(year = .year) %>%
                group_by(year) %>%
                do({
                    to_year <- .
                    total_graduates <- nrow(to_year)
                    total_founders <- to_year %>%
                filter(was_founder, year_founder <= .year) %>%
                    nrow()
                total_engineers <- to_year %>%
                filter(was_engineer, year_engineer <= .year) %>%
                nrow()
                data_frame(total_graduates = total_graduates,
                   total_founders = total_founders,
                   total_engineers = total_engineers)
                })
            }) %>%
                ungroup() %>%
                    mutate(fraction_founders = total_founders / total_graduates,
                           fraction_engineers = total_engineers / total_graduates)

    # Converts to 'long format'
    df_2 %<>%
    select(year, starts_with("fraction_")) %>%
        tidyr::gather(key, value, -year) %>%
            separate(key, c("exclude", "type")) %>%
                select(-exclude)
    
    g <- ggplot(data = df_2, aes(x = year, y = value, linetype = type)) + 
        geom_line() +
            theme_bw() +
                xlab("Year of graduation from Stanford") +
                    ylab("Fraction of total graduation pool")

    g
}

#' Get college majors of founders
#'
#' Get college majors of founders
#'
#' @return data frame
#' @export

FounderMajor <- function(){
    data("yctc_degree")
    data("yctc_education")
    data("yctc_founders")
    data("yctc_person")
    data("yctc_major")

    founders <- yctc_founders
    major <- yctc_major
    degree <- yctc_degree
    person <- yctc_person
    education <- yctc_education
    
    pm <- left_join(degree, education,
                    by = c("DegreeId", "src")) %>%
        filter(undergraduate == TRUE) %>%
            inner_join(major, by = "MajorId") %>%
                select(PersonId = PersonID, Major = Field)

    all.persons <- left_join(founders, person, 
                             by = c(
                                 "Last.Name" = "Surname",
                                 "First.Name" = "Name",
                                 "src" = "src")) %>%
    inner_join(startups, by = c("Name", "src")) %>%
         inner_join(pm, by = "PersonId") %>%
         arrange(PersonId) %>%
         distinct()

   df.3a <- all.persons %>%
   group_by(Major) %>%
   summarise(n = n()) %>%
   arrange(desc(n)) %>%
   mutate(percentage = n / sum(n))

   df.3a$Major <- factor(df.3a$Major, levels = df.3a$Major)

   df.3a %<>% cbind(binom::binom.confint(x = c(df.3a$n),
                               n = sum(df.3a$n), 
                               conf.level = 0.95,
                               tol = 1e-8,
                               methods = "exact") %>%
                                   select(lower, upper)) %>%
                       mutate(lower_n = round(lower * sum(n)),
                              upper_n = round(upper * sum(n)))

    
    df.3a
 }

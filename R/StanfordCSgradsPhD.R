#' Returns a database of Stanford CS PhDs with details on career history
#'
#' Returns a database of Stanford CS PhDs with details on career history
#'
#' @export

StanfordCSgradsPhD <- function(){
    data("person")
    data("education")
    data("experience")
    data("id_list")

    GRADS <- inner_join(person, education, by = c("PersonId" = "PersonID")) %>%
        filter(SchoolId %in% id_list$stanford_id,
         MajorId %in% id_list$cs_id,
         DegreeId %in% id_list$phd_id,
         Ongoing == 0)
    
    founders <- GRADS %>%
        select(PersonId, GraduationYear = EndYear) %>%
            left_join(experience, by = c("PersonId" = "PersonID")) %>%
                mutate(
                    is_founder = TitleID %in% id_list$founder_id &
                        StartYear >= GraduationYear,
                    is_engineer = TitleID %in% id_list$engineer_id &
                        StartYear >= GraduationYear,
                    years_after_graduation = as.numeric(StartYear) -
                        as.numeric(GraduationYear)
                ) %>%
                    group_by(PersonId)
    
    founders %>% group_by(PersonId) %>% summarise(any_founding = any(is_founder))
}

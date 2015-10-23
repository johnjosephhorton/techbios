#' Individual choices about careers
#' 
#' Create a data set of individual choices about careers at different points in time
#' 
#' @return dataset
#' @export
#' 

StanfordCSPhDGradsCareerChoices <- function() {
    data("experience")
    data("person")
    data("id_list")
    
    GRADS <- inner_join(person, education, by = c("PersonId" = "PersonID")) %>%
        filter(SchoolId %in% id_list$stanford_id,
               MajorId %in% id_list$cs_id,
               DegreeId %in% id_list$phd_id,
               Ongoing == 0)
    
    career_choices <- GRADS %>%
        select(PersonId, GraduationYear = EndYear) %>%
        left_join(experience, by = c("PersonId" = "PersonID")) %>%
        inner_join(person, by = c("PersonId" = "PersonId")) %>%
            mutate(
                FullName = stri_c(Name, Surname, sep = " "),
                Choice = ifelse(TitleID %in% founder_id, "Founder", "Employee") %>%
            factor(levels = c("Employee", "Founder"))) %>%
         distinct() %>%
         group_by(PersonId) %>%
         arrange(FullName, StartYear, StartMonth) %>%
             do({
                 .person <- .    
                 .person %>%
                     mutate(PreviousJobs = 0:(n() - 1),
                            EntrepreneurialJobs = lag(cumsum(as.numeric(Choice) - 1), default = 0),
                            EmployeeJobs = PreviousJobs - EntrepreneurialJobs)
             }) %>%
                 select(Choice, 
                        TimeDecisionMade = StartYear,
                        ClassYear = GraduationYear,
                        FullName,
                        PreviousJobs,
                        EntrepreneurialJobs, 
                        EmployeeJobs)
   
    career_choices   
}

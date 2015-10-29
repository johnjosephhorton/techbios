#' Individual choices about careers
#' 
#' Create a data set of individual choices about careers at different points in time
#' 
#' @return dataset
#' @export
#' 

## library(techbios)

## data("experience")
## data("person")
## data("id_list")
## data("education")

## GRADS <- inner_join(person, education, by = c("PersonId" = "PersonID")) %>%
##         filter(SchoolId %in% id_list$stanford_id,
##                MajorId %in% id_list$cs_id,
##                DegreeId %in% id_list$phd_id,
##                Ongoing == 0)


## career_choices <- GRADS %>%
##         select(PersonId, GraduationYear = EndYear) %>%
##         left_join(experience, by = c("PersonId" = "PersonID")) %>%
##             inner_join(person, by = c("PersonId" = "PersonId")) %>%
##             mutate(
##                 FullName = stri_c(Name, Surname, sep = " "),
##                 Engineer = ifelse(TitleID %in% id_list$founder_id, 1, 0),
##                 Founder  = ifelse(TitleID %in% id_list$engineer_id, 1, 0)
##             )

StanfordCSPhDGradsCareerChoices <- function() {
    data("experience")
    data("person")
    data("id_list")
    data("education")
    
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
                Engineer = ifelse(TitleID %in% id_list$founder_id, 1, 0),
                Founder  = ifelse(TitleID %in% id_list$engineer_id, 1, 0) 
            ) %>%
         distinct() %>%
         group_by(PersonId) %>%
         arrange(FullName, StartYear, StartMonth) %>%
             do({
                 .person <- .    
                 .person %>%
                     mutate(
                         PreviousJobs = 0:(n() - 1),
                         EntrepreneurialJobs = cumsum(Founder),
                         EngineerJobs = cumsum(Engineer)
                     )
             }) %>%
                 select(Engineer,
                        Founder, 
                        TimeDecisionMade = StartYear,
                        ClassYear = GraduationYear,
                        FullName,
                        PreviousJobs,
                        EntrepreneurialJobs, 
                        EngineerJobs)
   
    career_choices   
}

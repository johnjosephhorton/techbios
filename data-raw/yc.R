#!/usr/bin/Rscript --vanilla 

library(RSQLite)
library(binom)
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(plyr)
library(stringi)
library(stringr)
library(tidyr)


db.yc <- src_sqlite("yc_founders_bios.db")
startups <- read.table("yc_companies.csv", header = TRUE, sep = ",") %>% tbl_df
founders <- read.table("yc_founders.csv", header = TRUE, sep = ",") %>% tbl_df
degree <- tbl(src = db.yc, "Degree")
education <- tbl(src = db.yc, "Education")
major <- tbl(src = db.yc, "Major")
person <- tbl(src = db.yc, "Person")


cs.string <- "(Computer)|(Computing)|(Software)|(Information)|(Informatica)|(Informatics)"
stem.string <- "(Engineering)|(Physics)|(Mathematics)|(Technology)|(Medicine)|(Biophysics)"

founder_educ_backgrounds <-
    inner_join(person, education, by = "PersonId") %>%
    inner_join(., major, by = c("MajorId")) %>%
    select(First.Name = Name, Last.Name = Surname, Major) %>%
    collect %>%
    inner_join(., founders, by = c("First.Name", "Last.Name")) %>%
    mutate(
      CS = str_detect(Major, cs.string),
      STEM = str_detect(Major, stem.string)
    )

devtools::use_data(founder_educ_backgrounds, pkg = "..")

## founders.backgrounds %>%
##     group_by(First.Name, Last.Name) %>% 
##     summarize(
##         CS.background = any(CS),
##         STEM.background = any(STEM),
##         TECH = any(CS) | any(STEM)
##     ) %>% summary


## by.startup <- founders.backgrounds %>%
##     group_by(Name) %>% 
##     summarize(
##         CS.background = any(CS),
##         STEM.background = any(STEM),
##         TECH = any(CS) | any(STEM)
##     ) %>% summary %>% print


## founders.any.cs <- inner_join(person, education, by = "PersonId") %>%
## inner_join(., major, by = c("MajorId")) %>%
## select(First.Name = Name, Last.Name = Surname, Major) %>%
## collect %>%
## inner_join(., founders, by = c("First.Name", "Last.Name")) %>% 
## mutate(CS = str_detect(Major, cs.string)) %>%
## group_by(Name) %>% 
## summarize(CS.background = any(CS)) 

## mean(founders.any.cs$CS.background)

## founders.any.cs <- 
##     inner_join(person, education, by = "PersonId") %>%
##     inner_join(., major, by = c("MajorId")) %>%
##     select(FirstName = Name, Surname, Major) %>%
##     collect %>%
##     mutate(CS = str_detect(Major, cs.string)) %>%
##     group_by(FirstName, Surname) %>% 
##     summarize(CS.background = any(CS))



               

## db.tc <- src_sqlite("tc_founders_bios.db")

## .startups.path <- c("yc_companies.csv", "tc_companies.csv")
## startups <- 
##     rbind(read.table(.startups.path[1], header = TRUE, sep = ",") %>% 
##               mutate(src = "yc"),
##           read.table(.startups.path[2], header = TRUE, sep = ",") %>% 
##               mutate(src = "tc")
##           ) %>% tbl_df()

## .founders.path <- c("yc_founders.csv", "tc_founders.csv")
## founders <- 
##     rbind(read.table(.founders.path[1], header = TRUE, sep = ",", 
##                      na.strings = "n/a") %>%
##            mutate(src = "yc"),
##           read.table(.founders.path[2], header = TRUE, sep = ",", 
##                      na.strings = "n/a") %>%
##                          mutate(src = "tc")
##           ) %>% tbl_df()

## # Create DB tables connections-------------------------------------------------
## LoadTables <- function(.name, .db.yc = db.yc, .db.tc = db.tc) {
##   # Helper function to load combined tables from two source databases
##   #
##   # Args:
##   #  .name: table name to load
##   #  .db.yc: Y Combinator DB connection object
##   #  .db.tc: TechCrunch DB connectin object
##   #
##   # Returns:
##   #   Combined tables
  
##   rbind(
##     tbl(.db.yc, .name) %>% collect() %>% mutate(src = "yc"),
##     tbl(.db.tc, .name) %>% collect() %>% mutate(src = "tc"))
## }

## company <- LoadTables("Company")
## person <- LoadTables("Person")
## experience <- LoadTables("Experience")
## education <- LoadTables("Education")
## major <- LoadTables("Major")
## degree <- LoadTables("Degree")
## title <- LoadTables("Title")
## school <- LoadTables("School")

## # Dataset for "companies.tex"

## devtools::use_data(experience, pkg = "..", overwrite = TRUE)

## devtools::use_data(company, pkg = "..", overwrite = TRUE)
 
## library(stringr)

## # Imputes TC cohorts

## tc_cohorts <- 
##     read.table("tc_cohort.csv", sep = ",", header = TRUE, na.strings = "#N/A") %>% 
##      na.omit() %>%
##       mutate(year = stringr::str_match(Time.of.Techstar, "[0-9]{1,4}"),
##              season = stringr::str_match(Time.of.Techstar, "[[:alpha:]]{1,6}")
##              )


## tc_cohorts$season <- with(tc_cohorts, plyr::mapvalues(season, 
##                                                       from = c("Spring", "Fall", "Summer", NA, "Winter"), 
##                                                       to = c("S", "W", "S", "W", "W"))
##                           )

## tc_cohorts$class_code <- with(tc_cohorts, 
##                               stringr::str_c(season, stringr::str_sub(year, 3, 4)))


## tc_codes <- 
##   startups %>% 
##   inner_join(tc_cohorts, by = "Name") %>%
##   select(Name, class_code, src)

## startups %<>% 
##   left_join(tc_codes, by = c("Name", "src")) %>%
##   mutate(Class = ifelse(src == "yc", as.character(Class), class_code)) %>%
##   select(-class_code) %>%
##   na.omit()

## startups %<>%
##   tidyr::extract(Class, c("Season", "Year"), "([SWPF])([0-9]{2})") %>%
##   mutate(Year = 2000 + as.numeric(Year)) %>%
##   unite(cohort, Year, Season, remove = FALSE)

## founders %<>% na.omit()

## degree %<>%
##   mutate(undergraduate = 
##            stri_detect_regex(Degree, "(^BS)|(^B.S)|(^Bachelor)|(^BA)|(^B.A.)|(^B.)"))

## major %<>%
##   mutate(CS = 
##            str_detect(Major, "(Computer)|(Computing)|(Software)|(Information)|(Informatica)|(Informatics)"),
##          STEM = 
##            str_detect(Major, "(Engineering)|(Physics)|(Mathematics)|(Technology)|(Medicine)|(Biophysics)") & !CS,
##          Social =
##            str_detect(Major, "(Finance)|(Business)|(Economics)|(Economic)|(Public)|(Political)|(Accounting)|(Psychology)") & !CS & !STEM,
##          Other = !CS & !STEM & !Social) %>%
##   gather(Field, Value, -MajorId, -Major) %>%
##   filter(Value == TRUE) %>%
##   select(-Value) %>%
##   arrange(MajorId)

## education %<>%
##   mutate(DegreeId = as.integer(DegreeId),
##          MajorId = as.integer(MajorId),
##          StartYear = as.numeric(StartYear),
##          EndYear = as.numeric(EndYear))

## experience %<>%
##   mutate(StartYear = ifelse(StartYear != "", as.numeric(StartYear), lubridate::year(Sys.Date())),
##          EndYear = as.numeric(
##            ifelse(EndYear %in% c("Present", "", "actuel", "Actualidad", "Heute"),
##                   lubridate::year(Sys.Date()), EndYear)),
##          StartMonth = ifelse(!StartMonth %in% month.name, "January", StartMonth),
##          EndMonth = ifelse(!EndMonth %in% month.name, "December", EndMonth)) %>%
##   rowwise() %>%
##   mutate(StartMonth = which(month.name %in% StartMonth),
##          EndMonth = which(month.name %in% EndMonth)) %>%
##   mutate(years = (EndYear*12 + EndMonth - StartYear*12 - StartMonth + 1) / 12) %>%
##   ungroup() %>% 
##   tbl_df()

## title %<>%
##   mutate(Founder = 
##            str_detect(Title, "(Founder)|(founder)|(Owner)|(owner)|(Partner)|(Creator)"),
##          Business = str_detect(Title, "(CEO)|(CTO)|(VP)|(President)|(Vice)|(Product)") &
##            !Founder,
##          Designer = 
##            str_detect(Title, "(Designer)|(designer)|(Art )|(art )|(Creative)|(creative)|(Photographer)|(Graphic)|(Design)") &
##            !Founder & !Business,
##          Developer =
##            str_detect(Title, "(Engineer)|(Developer)|(Programmer)|(Software)") &
##            !Founder & !Business & !Designer,
##          Other = !Founder & !Business & !Designer & !Developer) %>%
##   gather(Field, Value, -src, -TitleId, -Title) %>%
##   filter(Value == TRUE) %>%
##   select(-Value) %>%
##   arrange(TitleId)

## yctc_startups <- startups
## yctc_person <- person
## yctc_founders <- founders
## yctc_education <- education
## yctc_title <- title
## yctc_experience <- experience
## yctc_degree <- degree
## yctc_major <- major

## devtools::use_data(yctc_startups, pkg = "..", overwrite = TRUE)
## devtools::use_data(yctc_person, pkg = "..", overwrite = TRUE)
## devtools::use_data(yctc_founders, pkg = "..", overwrite = TRUE)
## devtools::use_data(yctc_education, pkg = "..", overwrite = TRUE)
## devtools::use_data(yctc_title, pkg = "..", overwrite = TRUE)
## devtools::use_data(yctc_experience, pkg = "..", overwrite = TRUE)
## devtools::use_data(yctc_degree, pkg = "..",overwrite = TRUE)
## devtools::use_data(yctc_major, pkg = "..", overwrite = TRUE)


## school$SchoolID <- school$SchoolId

## df.school <- 
##     inner_join(education, school, by = c("SchoolId")) %>%
##        group_by(School) %>%
##        summarise(n = n()) %>%
##        arrange(desc(n))

## pm <- left_join(degree, education, by = c("DegreeId", "src")) %>%
##   filter(undergraduate == TRUE) %>%
##   inner_join(major, by = "MajorId") %>%
##   select(PersonId = PersonID, Major = Field)

## all.persons <- left_join(founders, person, 
##                          by = c("Last.Name" = "Surname", "First.Name" = "Name", "src" = "src")) %>%
##   inner_join(startups, by = c("Name", "src")) %>%
##   inner_join(pm, by = "PersonId") %>%
##   arrange(PersonId) %>%
##   distinct()

## df.3a <- all.persons %>%
##   group_by(Major) %>%
##   summarise(n = n()) %>%
##   arrange(desc(n)) %>%
##   mutate(percentage = n / sum(n))

## df.3a$Major <- factor(df.3a$Major, levels = df.3a$Major)


## g.major <- ggplot(data = df.3a) +
##   geom_bar(aes(x = factor(Major), y = n), stat = "identity", colour = "black", fill = "grey") + 
##   geom_errorbar(aes(x = factor(Major), ymin = lower_n, ymax = upper_n), width = 0.25) +
##   scale_x_discrete(labels = abbreviate(df.3a$Major, 20)) +
##   theme_bw() + 
##   xlab("Major") + 
##   ylab("Frequency")

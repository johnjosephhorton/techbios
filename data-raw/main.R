#!/usr/bin/Rscript --vanilla 

#-------------------------------------------------------------------------------
#
# Stanford CS PhD Graduates
# Upwork Contract #15366706
#
# Main code
#
# Code style follows 'Google R Style Guide'
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
#
#-------------------------------------------------------------------------------

rm(list = ls(all = TRUE))
gc(reset = TRUE)
set.seed(12345)

library(stringi)
library(plyr)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RSQLite)

# Data Setup--------------------------------------------------------------------

db <- dplyr::src_sqlite("stanford_cs_phd.db")

company <- tbl(db, "Company") %>% collect()
person <- tbl(db, "Person") %>% collect()
experience <- tbl(db, "Experience") %>% collect()
education <- tbl(db, "Education") %>% collect()
major <- tbl(db, "Major") %>% collect()
degree <- tbl(db, "Degree") %>% collect()
title <- tbl(db, "Title") %>% collect()
school <- tbl(db, "School") %>% collect()

# Data Pre-processing-----------------------------------------------------------

education %<>%
  mutate(StartYear = suppressWarnings(as.numeric(StartYear)),
         EndYear = suppressWarnings(as.numeric(EndYear)))

school %<>%
  mutate(School = 
           stri_replace_all_fixed(School, "\n", "") %>%
           stri_trim_both())

experience %<>%
  mutate(StartYear = suppressWarnings(as.numeric(StartYear)),
         EndYear = suppressWarnings(as.numeric(EndYear)))

# Stanford_ID
stanford_id <- school %>%
  filter(stri_detect_fixed(School, "Stanford")) %$%
  SchoolId

# CS_ID
cs_id <- major %>%
  filter(stri_detect_regex(Major, "(Computer)|(Computing)|(Software)|(Information)|(Informatica)|(Informatics)")) %$%
  MajorId

# PhD_ID
phd_id <- degree %>%
  mutate(phd = 
           stri_replace_all_regex(Degree, "[^a-zA-Z]", "") %>%
           stri_trans_tolower()) %>%
  filter(stri_detect_regex(phd, "(phd)|(doctorate)")) %$%
  DegreeId

# Founder_ID
founder_id <- title %>%
  filter(stri_detect_regex(Title, "(Founder)|(founder)|(Owner)|(owner)|(Partner)|(Creator)")) %$%
  TitleId

# Engineer ID
engineer_id <- title %>%
  filter(stri_detect_regex(Title, "(Engineer)|(Developer)|(Programmer)|(Software)|(Architect)")) %$%
  TitleId

# Get the list of Stanford CS PhD Graduates (GRADS)
GRADS <- 
  inner_join(person, education, by = c("PersonId" = "PersonID")) %>%
  filter(SchoolId %in% stanford_id,
         MajorId %in% cs_id,
         DegreeId %in% phd_id,
         Ongoing == 0)

# RESEARCH QUESTIONS------------------------------------------------------------
    
# Q1: Number of Stanford PhD graduates by year----------------------------------
df_1 <- GRADS %>%
  group_by(PersonId, Name, Surname) %>%
  summarise(StartYear = min(StartYear, na.rm = TRUE),
            EndYear = max(EndYear, na.rm = TRUE)) %>%
  ungroup() %>%
  na.omit() %>%
  group_by(EndYear) %>%
  summarise(NumGrads = n())

devtools::use_data(df_1, overwrite = TRUE)


# Q2: Fraction of graduates over time having title "founder" or "co-founder" in work histories post grad
founders <- GRADS %>%
  select(PersonId, GraduationYear = EndYear) %>%
  left_join(experience, by = c("PersonId" = "PersonID")) %>%
  mutate(
    is_founder = TitleID %in% founder_id & StartYear >= GraduationYear,
    is_engineer = TitleID %in% engineer_id & StartYear >= GraduationYear,
    years_after_graduation = StartYear - GraduationYear) %>%
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
  gather(key, value, -year) %>%
  separate(key, c("exclude", "type")) %>%
  select(-exclude)

devtools::use_data(df_2, overwrite = TRUE)


# Q3: Career Trajectory---------------------------------------------------------
GraduatesByYear <- 
  inner_join(person, education, by = c("PersonId" = "PersonID")) %>% 
  group_by(PersonId, Name, Surname) %>%
  summarize(GradYear = max(EndYear, na.rm = TRUE)) %>% 
  na.omit() %>%
  ungroup() %>%
  mutate(rank.order = rank(GradYear, ties.method = "random"))

df_3 <- inner_join(
  inner_join(GraduatesByYear, experience, by = c("PersonId" = "PersonID")),
  company, by = c("CompanyID" = "CompanyId")) %>%
  filter(StartYear >= GradYear) %>%
  mutate(type = ifelse(TitleID %in% engineer_id, "Engineer",
                       ifelse(TitleID %in% founder_id, "Founder", "Other"))) %>%
  group_by(PersonId) %>%
  do({
    person_data <- .
    person_data %>%
      mutate(any.founder = any(type %in% "Founder"),
             any.engineer = any(type %in% "Engineer"))
  })

devtools::use_data(df_3, overwrite = TRUE)


# Q4: Timeline of a single graduate---------------------------------------------

# Finds maximum year in the dataset
max_year <- max(experience$EndYear, na.rm = TRUE)

df_4 <- inner_join(experience, company, by = c("CompanyID" = "CompanyId")) %>%
  inner_join(title, by = c("TitleID" = "TitleId")) %>%
  inner_join(person, by = c("PersonID" = "PersonId")) %>%
  mutate(FullName = stri_c(Name, Surname, sep = " "),
         EndYear = mapvalues(EndYear, from = NA, to = max_year))

only_grads <- GRADS %>%
  select(PersonId, GraduateYear = EndYear) %>%
  na.omit()

df_4_limited <- df_4 %>%
  left_join(only_grads, by = c("PersonID" = "PersonId")) %>%
  filter(StartYear >= GraduateYear)

devtools::use_data(df_4, overwrite = TRUE)
devtools::use_data(df_4_limited, overwrite = TRUE)






## library(data.table)

## df.cohort <- data.table(founders)[, list(num.grads = .N,
##                                          num.founders = sum(was_founder), 
##                                          num.engineers = sum(was_engineer),
##                                          num.both = sum(was_founder & was_engineer),
##                                          frac.founders = sum(was_founder)/.N,
##                                          frac.engineers = sum(was_engineer)/.N
##                                          ), by  = graduation_year]

## ggplot(data = subset(df.cohort, num.grads > 6), aes(x = graduation_year, y = frac.founders)) + geom_line()

## ggplot(data = subset(df.cohort, num.grads > 6), aes(x = graduation_year, y = frac.engineers)) + geom_line()




## ggplot(data = df_2, aes(x = year, y = value)) +
##   geom_line(aes(colour = type), size = 1) +
##   ggtitle("Fraction of founders & engineeres") +
##   ylab("Fraction, % of total graduation pool") +
##   xlab("Year") +
##   theme_light() +
##   theme(legend.title = element_blank(),
##         legend.key = element_blank())
## ggsave(filename = "output/Q2_fraction_of_founders_engineers_by_year.png", width = 9, height = 6)



## #
## # THE CODE BELOW IS ADOPTED FROM PREVIOUS PROJECT
## #


## # library(survival)
## # library(lfe)
## # library(dplyr)
## # library(tidyr)
## # library(stringr)
## # library(lubridate)
## # library(ggplot2)
## # library(magrittr)
## # library(scales)
## # library(binom)
## # library(stargazer)
## # library(data.table)
## # library(JJHmisc) # non-cran resource

## # system("rm ../../writeup/parameters.tex")
## # addParam <- genParamAdder("../../writeup/parameters.tex")


## # df.company <- 
## #   inner_join(experience, company, 
## #              by = c("CompanyID" = "CompanyId")) %>%
## #   group_by(CompanyName) %>%
## #   summarise(n = length(unique(PersonID))) %>%
## #   arrange(desc(n)) %>%
## #   mutate(CompanyName = plyr::mapvalues(CompanyName, "McKinsey & Company", "McKinsey"))
## # 
## # stargazer(df.company %>% filter(n > 9 & !CompanyName %in% c("Stanford University")),
## #           title = "Employers of Stanford CS PhDs graduates",
## #           summary = FALSE,
## #           label = "tab:companies", 
## #           column.labels = c("Company", "N"), 
## #           out = "writeup/tables/companies.tex",
## #           rownames = FALSE)
## # system("sed -i 's/cc/lr/g' ../../writeup/tables/companies.tex")

## # educational data
## # school$SchoolID <- school$SchoolId

## # df.school <- 
## #   inner_join(education, school, by = c("SchoolId")) %>%
## #   group_by(School) %>%
## #   summarise(n = n()) %>%
## #   arrange(desc(n))
## # 
## # stargazer(df.school %>% filter(n > 15 & !School %in% c("Y Combinator\n", "YCombinator\n", "YC", "TechCrunch\n")),
## #           title = "Schools of Y Combinator / Techcrunch founders",
## #           summary = FALSE,
## #           label = "tab:schools", 
## #           column.labels = c("Company", "N"), 
## #           out = "../../writeup/tables/schools.tex",
## #           rownames = FALSE)
## # system("sed -i 's/cc/lr/g' ../../writeup/tables/schools.tex")

## # # Imputes TC cohorts
## # tc_cohorts <- 
## #   read.table("../../data/tc_cohort.csv", sep = ",", header = TRUE, na.strings = "#N/A") %>% 
## #   na.omit() %>%
## #   mutate(year = str_match(Time.of.Techstar, "[0-9]{1,4}"),
## #          season = str_match(Time.of.Techstar, "[[:alpha:]]{1,6}")) %>%
## #   mutate(season = plyr::mapvalues(season, 
## #                                   from = c("Spring", "Fall", "Summer", NA, "Winter"), 
## #                                   to = c("S", "W", "S", "W", "W")),
## #          class_code = str_c(season, str_sub(year, 3, 4))) %>%
## #   tbl_df()
## # 
## # tc_codes <- 
## #   startups %>% 
## #   inner_join(tc_cohorts, by = "Name") %>%
## #   select(Name, class_code, src)
## # 
## # startups %<>% 
## #   left_join(tc_codes, by = c("Name", "src")) %>%
## #   mutate(Class = ifelse(src == "yc", as.character(Class), class_code)) %>%
## #   select(-class_code) %>%
## #   na.omit()
## # 
## # startups %<>%
## #   tidyr::extract(Class, c("Season", "Year"), "([SWPF])([0-9]{2})") %>%
## #   mutate(Year = 2000 + as.numeric(Year)) %>%
## #   unite(cohort, Year, Season, remove = FALSE)
## # 
## # founders %<>% na.omit()
## # 
## # degree %<>%
## #   mutate(undergraduate = 
## #            stri_detect_regex(Degree, "(^BS)|(^B.S)|(^Bachelor)|(^BA)|(^B.A.)|(^B.)"))
## # 
## # major %<>%
## #   mutate(CS = 
## #            str_detect(Major, "(Computer)|(Computing)|(Software)|(Information)|(Informatica)|(Informatics)"),
## #          STEM = 
## #            str_detect(Major, "(Engineering)|(Physics)|(Mathematics)|(Technology)|(Medicine)|(Biophysics)") & !CS,
## #          Social =
## #            str_detect(Major, "(Finance)|(Business)|(Economics)|(Economic)|(Public)|(Political)|(Accounting)|(Psychology)") & !CS & !STEM,
## #          Other = !CS & !STEM & !Social) %>%
## #   gather(Field, Value, -MajorId, -Major) %>%
## #   filter(Value == TRUE) %>%
## #   select(-Value) %>%
## #   arrange(MajorId)
## # 
## # education %<>%
## #   mutate(DegreeId = as.integer(DegreeId),
## #          MajorId = as.integer(MajorId),
## #          StartYear = as.numeric(StartYear),
## #          EndYear = as.numeric(EndYear))
## # 
## # experience %<>%
## #   mutate(StartYear = ifelse(StartYear != "", as.numeric(StartYear), year(Sys.Date())),
## #          EndYear = as.numeric(
## #            ifelse(EndYear %in% c("Present", "", "actuel", "Actualidad", "Heute"),
## #                   year(Sys.Date()), EndYear)),
## #          StartMonth = ifelse(!StartMonth %in% month.name, "January", StartMonth),
## #          EndMonth = ifelse(!EndMonth %in% month.name, "December", EndMonth)) %>%
## #   rowwise() %>%
## #   mutate(StartMonth = which(month.name %in% StartMonth),
## #          EndMonth = which(month.name %in% EndMonth)) %>%
## #   mutate(years = (EndYear*12 + EndMonth - StartYear*12 - StartMonth + 1) / 12) %>%
## #   ungroup() %>% 
## #   tbl_df()
## # 
## # title %<>%
## #   mutate(Founder = 
## #            str_detect(Title, "(Founder)|(founder)|(Owner)|(owner)|(Partner)|(Creator)"),
## #          Business = str_detect(Title, "(CEO)|(CTO)|(VP)|(President)|(Vice)|(Product)") &
## #            !Founder,
## #          Designer = 
## #            str_detect(Title, "(Designer)|(designer)|(Art )|(art )|(Creative)|(creative)|(Photographer)|(Graphic)|(Design)") &
## #            !Founder & !Business,
## #          Developer =
## #            str_detect(Title, "(Engineer)|(Developer)|(Programmer)|(Software)") &
## #            !Founder & !Business & !Designer,
## #          Other = !Founder & !Business & !Designer & !Developer) %>%
## #   gather(Field, Value, -src, -TitleId, -Title) %>%
## #   filter(Value == TRUE) %>%
## #   select(-Value) %>%
## #   arrange(TitleId)
## # 
## # # Questions--------------------------------------------------------------------
## # 
## # # Q1---------------------------------------------------------------------------
## # # Show number of Y combinator companies per cohort
## # df.1 <- startups %>%
## #   filter(src == "yc") %>%
## #   group_by(cohort) %>%
## #   summarise(startups = n()) %>%
## #   ungroup()
## # 
## # g.per.yc.cohort <- ggplot(data = df.1) +
## #   geom_line(aes(x = 1:nrow(df.1), y = startups)) +
## #   scale_x_continuous(breaks = 1:nrow(df.1), labels = df.1$cohort) +
## #   theme_bw() +
## #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
## #   xlab("Cohort") + ylab("Number of Start-Ups")
## # 
## # writeImage(g.per.yc.cohort, "per_yc_cohort", width = 7, height = 4)
## # 
## # # Q2---------------------------------------------------------------------------
## # # Show mean founder team size per cohort
## # df.2 <- inner_join(founders, startups, by = c("Name", "src")) %>%
## #   group_by(cohort, Name) %>%
## #   summarise(founders = n()) %>%
## #   group_by(cohort) %>%
## #   summarise(mean_team = mean(founders),
## #             max_team = max(founders),
## #             min_team = min(founders), 
## #             se = sd(founders) / sqrt(n())) %>%
## #   mutate(lower = mean_team - 2*se,
## #          upper = mean_team + 2*se)
## # 
## # g.team.size <- ggplot(data = df.2, aes(x = 1:nrow(df.2))) +
## #   geom_line(aes(y = mean_team)) +
## #   geom_line(aes(y = max_team), colour = "red") +
## #   geom_line(aes(y = min_team), colour = "blue") + 
## #   geom_errorbar(aes(ymax = upper, ymin = lower),
## #                 alpha = 0.4, width = 0.3) +
## #   scale_x_continuous(breaks = 1:nrow(df.2), labels = df.2$cohort) +
## #   theme_bw() +
## #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
## #   xlab("Cohort") + ylab("Mean Team Size")
## # 
## # writeImage(g.team.size, "team_size", width = 7, height = 4)
## # 
## # 
## # # Q3---------------------------------------------------------------------------
## # # Plot overall distribution of undergraduate majors of all founders 
## # # and also faceted by cohort
## # 
## # pm <- left_join(degree, education, by = c("DegreeId", "src")) %>%
## #   filter(undergraduate == TRUE) %>%
## #   inner_join(major, by = "MajorId") %>%
## #   select(PersonId = PersonID, Major = Field)
## # 
## # all.persons <- left_join(founders, person, 
## #                          by = c("Last.Name" = "Surname", "First.Name" = "Name", "src" = "src")) %>%
## #   inner_join(startups, by = c("Name", "src")) %>%
## #   inner_join(pm, by = "PersonId") %>%
## #   arrange(PersonId) %>%
## #   distinct()
## # 
## # df.3a <- all.persons %>%
## #   group_by(Major) %>%
## #   summarise(n = n()) %>%
## #   arrange(desc(n)) %>%
## #   mutate(percentage = n / sum(n))
## # 
## # df.3a$Major <- factor(df.3a$Major, levels = df.3a$Major)
## # 
## # df.3a %<>% cbind(binom.confint(x = c(df.3a$n),
## #                                n = sum(df.3a$n), 
## #                                conf.level = 0.95,
## #                                tol = 1e-8,
## #                                methods = "exact") %>%
## #                    select(lower, upper)) %>%
## #   mutate(lower_n = round(lower * sum(n)),
## #          upper_n = round(upper * sum(n)))
## # 
## # g.major <- ggplot(data = df.3a) +
## #   geom_bar(aes(x = factor(Major), y = n), stat = "identity", colour = "black", fill = "grey") + 
## #   geom_errorbar(aes(x = factor(Major), ymin = lower_n, ymax = upper_n), width = 0.25) +
## #   scale_x_discrete(labels = abbreviate(df.3a$Major, 20)) +
## #   theme_bw() + 
## #   xlab("Major") + 
## #   ylab("Frequency")
## # 
## # writeImage(g.major, "college_major", width = 7, height = 3)
## # 
## # df.3b <- all.persons %>%
## #   group_by(Major, cohort) %>%
## #   summarise(n = n()) %>%
## #   ungroup() %>%
## #   arrange(cohort, desc(n)) %>%
## #   group_by(cohort) %>%
## #   mutate(percentage = n / sum(n))
## # 
## # df.3b$Major <- factor(df.3b$Major, levels = df.3a$Major)
## # df.3b$cohort_level <- as.numeric(factor(df.3b$cohort))
## # 
## # g.college.major.by.cohort <- ggplot(data = df.3b) +
## #   geom_bar(aes(x = factor(Major), y = n), stat = "identity", alpha = 0.9) + 
## #   facet_wrap(~ cohort) + 
## #   theme_bw() + 
## #   xlab("Major") + 
## #   ylab("Frequency")
## # 
## # writeImage(g.college.major.by.cohort, "college_major_by_cohort", width = 7, height = 3)
## # 
## # g.college.major.by.cohort.lines <- ggplot(data = df.3b) + 
## #   theme_bw() +
## #   geom_line(aes(x = cohort_level, y = n, colour = Major)) +
## #   scale_x_discrete(breaks = 1:length(unique(df.3b$cohort)), labels = unique(df.3b$cohort)) +
## #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
## #   xlab("Cohort") + ylab("Frequency")
## # 
## # writeImage(g.college.major.by.cohort.lines, "college_major_by_cohort_lines", width = 7, height = 3)
## # 
## # # Q4---------------------------------------------------------------------------
## # # Plot years of work experience at time of start of y combinator session 
## # # and faceted by cohort
## # 
## # df.4 <- inner_join(all.persons, experience, by = c("PersonId" = "PersonID", "src" = "src")) %>%
## #   mutate(Start = ifelse(Season == "Winter", 12, 6)) %>%
## #   group_by(PersonId, cohort) %>%
## #   filter((Year*12 + Start) > (StartYear*12 + StartMonth)) %>%
## #   mutate(fraction = ifelse((EndYear*12 + EndMonth) <= (Year*12 + Start), 
## #                            years,
## #                            (Year*12 + Start - StartYear*12 - StartMonth) / 12)) %>%
## #   summarise(prior = min(sum(fraction), max(EndYear) - min(StartYear) + 1))
## # 
## # df.4 <- left_join(all.persons, df.4, by = c("PersonId", "cohort")) %>%
## #   select(PersonId, cohort, prior) %>%
## #   distinct()
## # 
## # g.founder.experience <- ggplot(data = df.4) +
## #   geom_histogram(aes(x = prior), binwidth = 3, alpha = 0.9, fill = "gray", colour = "black") +
## #   xlab("Years of experience") + 
## #   ylab("Frequency") +
## #   theme_bw()
## # 
## # writeImage(g.founder.experience, "founder_experience", width = 7, height = 3)
## # 
## # df.4$cohort_level <- as.numeric(factor(df.4$cohort))
## # 
## # g.founder.experience.by.cohort <- ggplot(data = df.4) +
## #   geom_histogram(aes(x = prior), binwidth = 3, alpha = 0.9) +
## #   facet_wrap(~ cohort) +
## #   xlab("Years of experience") + 
## #   ylab("Frequency") +
## #   theme_bw()
## # 
## # writeImage(g.founder.experience.by.cohort, "founder_experience_by_cohort", width = 7, height = 3)
## # 
## # df.4 %<>%
## #   group_by(cohort) %>%
## #   mutate(q25 = quantile(prior, c(.25), na.rm = TRUE),
## #          q50 = quantile(prior, c(.50), na.rm = TRUE),
## #          q75 = quantile(prior, c(.75), na.rm = TRUE),
## #          mean = mean(prior, na.rm = TRUE)) %>%
## #   na.omit()
## # 
## # g.founder.experience.by.cohort.lines <- ggplot(data = df.4) +
## #   theme_bw() +
## #   geom_point(aes(x = cohort_level, y = prior)) +
## #   geom_ribbon(aes(x = cohort_level, ymin = q25, ymax = q75), 
## #               alpha = 0.5, fill = "lightgreen") +
## #   geom_line(aes(x = cohort_level, y = q50, colour = "50%")) +
## #   geom_line(aes(x = cohort_level, y = mean, colour = "Mean"), linetype = "dashed") + 
## #   scale_x_discrete(breaks = 1:length(unique(df.4$cohort)), labels = unique(df.4$cohort)) +
## #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
## #   theme(legend.title = element_blank(), legend.key = element_blank()) + 
## #   xlab("Cohort\n* Green area is 25% and 75% percentiles") +
## #   ylab("Years of experience") + 
## #   ggtitle("Experience of co-founders by cohort")
## # 
## # writeImage(g.founder.experience.by.cohort.lines, "founder_experience_by_cohort_lines", width = 7, height = 3)
## # 
## # # Q5---------------------------------------------------------------------------
## # # What fraction of founders specifically majored in CS?
## # 
## # df.3a$n[df.3a$Major == "CS"] / sum(df.3a$n)
## # 
## # df.5 <- df.3b %>%
## #   group_by(cohort) %>%
## #   mutate(CS = ifelse(Major == "CS", 1, 0)) %>%
## #   summarise(fraction = sum(CS * n) / sum(n))
## # 
## # g.cs.majors <- ggplot(data = df.5) +
## #   geom_line(aes(x = 1:nrow(df.5), y = fraction)) +
## #   scale_x_continuous(breaks = 1:nrow(df.5), labels = df.5$cohort) +
## #   scale_y_continuous(label = percent, limits = c(0,1)) +
## #   theme_bw() +
## #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
## #   xlab("Cohort") +
## #   ylab("Fraction of founders with degree in CS") 
## # 
## # 
## # writeImage(g.cs.majors, "cs_majors", width = 7, height = 3)
## # 
## # 
## # # Q6---------------------------------------------------------------------------
## # # Create a visualization that shows the timeline of a single 
## # # Y combinator start-up - birth, founders leaving, death and exit
## # 
## # .person <- 339
## # 
## # df.6 <- inner_join(experience, company, by = c("CompanyID" = "CompanyId", "src" = "src")) %>%
## #   inner_join(title, by = c("TitleID" = "TitleId", "src" = "src")) %>%
## #   filter(PersonID == .person)
## # 
## # .title <- person %>% 
## #   filter(PersonId == .person) %$%
## #   str_c(Name, Surname, sep = " ")
## # 
## # g.person <- ggplot(data = df.6) +
## #   geom_rect(aes(xmin = StartYear, xmax = EndYear,
## #                 ymin = ExperienceID, ymax = ExperienceID + 1), fill = "grey", alpha = 0.5) +
## #   geom_text(aes(x = (StartYear + EndYear)/2, y = ExperienceID + 3/4, label = CompanyName)) +
## #   geom_text(aes(x = (StartYear + EndYear)/2, y = ExperienceID + 1/4, label = Title)) +
## #   theme_bw() + 
## #   theme(axis.text.y = element_blank()) + 
## #   xlab("Year") + ylab("") + ggtitle(.title)
## # 
## # print(g.person)
## # 
## # 
## # # Q7---------------------------------------------------------------------------
## # # What fraction of founders worked as a software engineer, developer, programmer
## # # or equivalent prior to Y combinator?
## # 
## # df.7 <- inner_join(all.persons, experience, by = c("PersonId" = "PersonID", "src" = "src")) %>%
## #   mutate(Start = ifelse(Season == "Winter", 12, 6)) %>%
## #   group_by(PersonId) %>%
## #   filter((Year*12 + Start) > (StartYear*12 + StartMonth)) %>%
## #   inner_join(title, by = c("TitleID" = "TitleId", "src" = "src")) %>%
## #   summarise(was_developer = "Developer" %in% Field) %>%
## #   group_by(was_developer) %>%
## #   summarise(n = n())
## # 
## # df.7$percentage <- df.7$n / sum(df.7$n)
## # df.7
## # 
## # g.was.developer <- ggplot(data = df.7) +
## #   geom_bar(aes(x = was_developer, y = percentage, width = 0.5), stat = "identity", alpha = 0.9) +
## #   theme_bw() +
## #   xlab("Was developer prior Y-Combinator?") + ylab("Percentage")
## # 
## # writeImage(g.was.developer, "was_developer", width = 7, height = 3)
## # 
## # 
## # 
## # # Q8---------------------------------------------------------------------------
## # # What fraction of founders worked as a software engineer, developer, programmer 
## # # or equivalent after Y combinator?
## # 
## # df.8 <- inner_join(all.persons, experience, by = c("PersonId" = "PersonID", "src" = "src")) %>%
## #   mutate(Start = ifelse(Season == "Winter", 12, 6)) %>%
## #   group_by(PersonId) %>%
## #   filter((Year*12 + Start + 3) < (StartYear*12 + StartMonth)) %>%
## #   inner_join(title, by = c("TitleID" = "TitleId", "src" = "src")) %>%
## #   summarise(become_developer = "Developer" %in% Field) %>%
## #   group_by(become_developer) %>%
## #   summarise(n = n())
## # 
## # df.8$percentage <- df.8 %$% n / sum(df.8$n)
## # df.8
## # 
## # g.later.developer <- ggplot(data = df.8) +
## #   geom_bar(aes(x = become_developer, y = percentage, width = 0.5), stat = "identity", alpha = 0.9) +
## #   theme_bw() +
## #   xlab("Become developer after Y-Combinator?") + ylab("Percentage")
## # 
## # writeImage(g.later.developer, "later_developer", width = 7, height = 3)
## # 
## # 
## # 
## # # Q9---------------------------------------------------------------------------
## # # What fraction of founder teams have a founder who was either a engineer of some kind or
## # # had an undergraduate or graduate degree in a technical field?
## # 
## # df.9 <- inner_join(all.persons, experience, by = c("PersonId" = "PersonID", "src" = "src")) %>%
## #   mutate(Start = ifelse(Season == "Winter", 12, 6)) %>%
## #   group_by(PersonId, First.Name, Last.Name, Name) %>%
## #   filter((Year*12 + Start) > (StartYear*12 + StartMonth)) %>%
## #   inner_join(title, by = c("TitleID" = "TitleId", "src" = "src")) %>%
## #   summarise(technical_guy = "Developer" %in% Field | "CS" %in% Major | "STEM" %in% Major) %>%
## #   group_by(Name) %>%
## #   summarise(has_technical_guys = sum(technical_guy) > 0) %>%
## #   group_by(has_technical_guys) %>%
## #   summarise(percentage = n()) %>%
## #   mutate(percentage = percentage / sum(percentage))
## # 
## # df.9
## # 
## # g.any.eng <- ggplot(data = df.9) +
## #   geom_bar(aes(x = has_technical_guys, y = percentage, width = 0.5), stat = "identity", alpha = 0.9) +
## #   theme_bw() +
## #   xlab("Has engineer or co-founder with technical degree?") + ylab("Percentage")
## # 
## # writeImage(g.any.eng, "any_eng", width = 7, height = 3)
## # 
## # 
## # hasEng <- subset(df.9, has_technical_guys)$percentage
## # addParam("hasEng", 100*round(hasEng, 2))
## # 
## # # Q10--------------------------------------------------------------------------
## # # For each Y combinator cohort, show the fractions of founders that: 
## # # (a) are still at their original start-up 
## # # (b) are the founder/co-founder of another start-up
## # # (c) have an employee role at some other company
## # 
## # df.10 <- inner_join(experience, company, by = c("CompanyID" = "CompanyId", "src" = "src")) %>%
## #   inner_join(title, by = c("TitleID" = "TitleId", "src" = "src")) %>%
## #   inner_join(all.persons, by = c("PersonID" = "PersonId", "src" = "src")) %>%
## #   mutate(Start = ifelse(Season == "Winter", 12, 6)) %>%
## #   select(cohort, Year, Season, Start, PersonID, Name, CompanyName, StartYear, StartMonth, EndYear, EndMonth, Title, Field) %>%
## #   distinct() %>%
## #   mutate(after_startup = (EndYear*12 + EndMonth) > (Year*12 + Start)) %>%
## #   mutate(still_in_startup = (Name == CompanyName) & after_startup,
## #          founder_of_another_startup = (Name != CompanyName) & (CompanyName %in% startups$Name) & after_startup,
## #          employee_at_other_company = (Name != CompanyName) & !(CompanyName %in% startups$Name) & (Field != "Founder") & after_startup) %>%
## #   group_by(cohort, PersonID) %>%
## #   summarise(still_in_startup = any(still_in_startup),
## #             founder_of_another_startup = any(founder_of_another_startup),
## #             employee_at_other_company = any(employee_at_other_company)) %>%
## #   group_by(cohort) %>%
## #   summarise(employee_at_other_company = sum(employee_at_other_company) / n(),
## #             still_in_startup = sum(still_in_startup) / n(),
## #             founder_of_another_startup = sum(founder_of_another_startup) / n()) 
## # 
## # df.10
## # 
## # df.10 %<>% gather(key, value, -cohort)
## # .x <- rep(1:(nrow(df.10)/3), 3)
## # 
## # pretty.roles <- list("employee_at_other_company" = "Employee at another company", "still_in_startup" = "At original startup", "founder_of_another_startup" = "Founder of another startup")
## # df.10$role <- with(df.10, as.vector(unlist(pretty.roles[key])))
## # 
## # 
## # g.where.now <- ggplot(data = df.10) +
## #   geom_bar(aes(x = .x, y = value), stat = "identity", position = "dodge", colour = "black", fill = "grey") +
## #   facet_wrap(~role) + 
## #   scale_x_continuous(breaks = 1:nrow(df.1), labels = df.1$cohort) +
## #   theme_bw() +
## #   xlab("Cohort") + ylab("Fraction of founders") + coord_flip()  
## # 
## # writeImage(g.where.now, "where_now", width = 9, height = 6)
## # 
## # 
## # # Q11--------------------------------------------------------------------------
## # # Of the employees at some other company, what fraction are: 
## # # (a) Engineers/developers of some kind
## # # (b) In a business role (CEO, CTO, VP, Product Manager) 
## # # (c) Other
## # 
## # df.11 <- inner_join(experience, company, by = c("CompanyID" = "CompanyId", "src" = "src")) %>%
## #   inner_join(title, by = c("TitleID" = "TitleId", "src" = "src")) %>%
## #   inner_join(all.persons, by = c("PersonID" = "PersonId")) %>%
## #   mutate(Start = ifelse(Season == "Winter", 12, 6)) %>%
## #   select(cohort, Year, Season, Start, PersonID, Name, CompanyName, StartYear, StartMonth, EndYear, EndMonth, Title, Field) %>%
## #   distinct() %>%
## #   mutate(after_startup = (EndYear*12 + EndMonth) > (Year*12 + Start)) %>%
## #   mutate(employee_at_other_company = (Name != CompanyName) & !(CompanyName %in% startups$Name) & (Field != "Founder") & after_startup) %>%
## #   filter(employee_at_other_company == TRUE) %>%
## #   group_by(cohort) %>%
## #   summarise(Developers = sum(Field %in% c("Developer", "Designer")) / n(),
## #             Business = sum(Field == "Business") / n(),
## #             Other = sum(Field == "Other") / n())
## # 
## # df.11
## # 
## # df.11 %<>% gather(key, value, -cohort)
## # .x <- rep(1:(nrow(df.11)/3), 3)
## # 
## # g.other.role <- ggplot(data = df.11) +
## #   geom_bar(aes(x = .x, y = value), stat = "identity", position = "dodge") +
## #   facet_wrap(~key) + 
## #   scale_x_continuous(breaks = 1:nrow(df.1), labels = df.1$cohort) +
## #   theme_bw() +
## #   xlab("Cohort") +
## #   ylab("Employee roles in other companies") + coord_flip()  
## # 
## # print(g.other.role)
## # 
## # writeImage(g.other.role, "other_role", width = 7, height = 6)
## # 
## # 
## # # Q12--------------------------------------------------------------------------
## # # Fit a hazard model at the level of start-up, where a “death” is that a 
## # # majority of founder/co-founders have changed firms. Time should be from the 
## # # start of their Y Combinator session. 
## # #
## # # (a) Test the hypothesis that all cohorts have the same hazard rate. 
## # # (b) Plot survival curves pooled across cohorts
## # # (c) Plot survival curves by cohort
## # 
## # persons <- inner_join(experience, company, by = c("CompanyID" = "CompanyId", "src" = "src")) %>%
## #   inner_join(title, by = c("TitleID" = "TitleId", "src" = "src"))
## # 
## # death <- function(df) {
## #   # Helper function to calculate 'death' statistics
## #   #
## #   # Args:
## #   #  df: data.frame for single company
## #   #
## #   # Returns:
## #   #  death - logical
## #   #  months - lifetime
## #   
## #   .cohort <- df$cohort[1]
## #   .is_left <- as.numeric(sum(df$is_left) / nrow(df))
## #   .months <- df %>% 
## #     filter(is_left == .is_left) %>%
## #     summarise(months = max(months))
## #   
## #   data_frame(cohort = .cohort, death = .is_left, months = .months$months)
## # }
## # 
## # df.12 <- 
## #   founders %>% 
## #   inner_join(startups, by = c("Name", "src")) %>%
## #   left_join(persons, by = c("Name" = "CompanyName", "src" = "src")) %>% 
## #   filter(Field %in% c("Founder", NA)) %>%
## #   mutate(start = str_c(Year, ifelse(Season == "S", "06", "12"), "01", sep = "-") %>% ymd()) %>%
## #   na.omit() %>%
## #   group_by(Name, start, Last.Name, First.Name) %>%
## #   summarise(cohort = first(cohort),
## #             is_left = !first(Ongoing),
## #             period_left = str_c(first(EndYear), 
## #                                 ifelse(is_left, first(EndMonth), sprintf("%02d", month(Sys.Date()))),
## #                                 "01", sep = "-") %>% ymd()) %>%
## #   mutate(months = days(period_left - start) %>% day() %>% `%/%`(30)) %>%
## #   mutate(months = ifelse(months > 0, months, 0)) %>%
## #   group_by(Name) %>%
## #   do(death(.)) %>%
## #   ungroup()
## # 
## # df.12
## # 
## # # (b)
## # fit.1 <- survfit(Surv(months, death) ~ 1, data = df.12)
## # summary(fit.1)
## # plot(fit.1)
## # 
## # fit.1.plot <- fit.1 %$% 
## #   data_frame(time, surv, lower, upper)
## # 
## # g.surv.pooled <- ggplot(data = fit.1.plot, aes(x = time)) +
## #   geom_step(aes(y = surv), size = 1, direction = "vh") + 
## #   geom_step(aes(y = lower), linetype = "dashed", direction = "vh") +
## #   geom_step(aes(y = upper), linetype = "dashed", direction = "vh") + 
## #   theme_bw() + 
## #   xlab("Time, months") + ylab("Survival rate")
## # 
## # print(g.surv.pooled)
## # writeImage(g.surv.pooled, "surv_pooled", width = 7, height = 6)
## # 
## # # (c)
## # fit.2 <- survfit(Surv(months, death) ~ cohort, data = df.12)
## # sum.2 <- summary(fit.2)
## # sum.2
## # plot(fit.2)
## # 
## # fit.2.plot <- sum.2 %$% 
## #   data_frame(time, surv, strata) %>%
## #   mutate(strata = str_extract(strata, "[0-9]{4}_[SW]"))
## # 
## # has.data <- fit.2.plot %>%
## #   group_by(strata) %>%
## #   summarise(n = n()) %>%
## #   filter(n > 1) %>%
## #   select(-n)
## # 
## # fit.2.plot %<>% filter(strata %in% has.data$strata)
## # 
## # g.surv <- ggplot(data = fit.2.plot, aes(x = time)) +
## #   geom_step(aes(y = surv, colour = strata), direction = "vh") + 
## #   xlab("Time, months") + ylab("Survival rate") + 
## #   guides(colour = guide_legend(ncol = 2)) + 
## #   theme_bw()
## # 
## # print(g.surv)
## # writeImage(g.surv, "surv", width = 7, height = 6)
## # 
## # # (a)
## # 
## # # Whole sample
## # dif <- survdiff(Surv(months, death) ~ cohort, data = df.12)
## # dif
## # 
## # # Just two cohorts
## # dif <- survdiff(Surv(months, death) ~ cohort, 
## #                 data = df.12 %>% filter(cohort %in% c("2009_S", "2012_S")))
## # dif
## # 
## # # Q13--------------------------------------------------------------------------
## # # From the basic model is 12, add as covariates: 
## # # (a) Average work experience of founders
## # # (b) Average years of education 
## # # (c) Fraction of founders with prior startup founder or co-founder experience 
## # 
## # df.13 <- df.12
## # 
## # # Imputes average prior experience of founders
## # prior_expirience <- inner_join(all.persons, 
## #                                experience, by = c("PersonId" = "PersonID", "src" = "src")) %>%
## #   mutate(Start = ifelse(Season == "Winter", 12, 6)) %>%
## #   group_by(PersonId, cohort, Name) %>%
## #   filter((Year*12 + Start) > (StartYear*12 + StartMonth)) %>%
## #   mutate(fraction = ifelse((EndYear*12 + EndMonth) <= (Year*12 + Start), 
## #                            years,
## #                            (Year*12 + Start - StartYear*12 - StartMonth) / 12)) %>%
## #   summarise(experience = min(sum(fraction), max(EndYear) - min(StartYear) + 1)) %>%
## #   group_by(Name, cohort) %>%
## #   summarise(experience = mean(experience))
## # 
## # df.13 %<>% left_join(prior_expirience, by = c("Name", "cohort"))
## # 
## # # Imputes average years of education
## # prior_education <- founders %>% 
## #   inner_join(startups, by = c("Name", "src")) %>%
## #   left_join(persons, by = c("Name" = "CompanyName", "src" = "src")) %>% 
## #   filter(Field == "Founder") %>%
## #   select(Name, PersonID, Year) %>%
## #   left_join(education, by = c("PersonID")) %>%
## #   mutate(education = ifelse(Year > EndYear, EndYear - StartYear + 1,
## #                             ifelse(Year < StartYear, 0,
## #                                    Year - StartYear + 1))) %>%
## #   group_by(Name) %>%
## #   summarise(education = mean(education, na.rm = TRUE))
## # 
## # df.13 %<>% left_join(prior_education, by = "Name")
## # 
## # # Imputes prior startup founder or co-founder experience
## # prior_founder <- 
## #   inner_join(all.persons, experience, by = c("PersonId" = "PersonID", "src" = "src")) %>%
## #   mutate(Start = ifelse(Season == "Winter", 12, 6)) %>%
## #   filter((Year*12 + Start) > (StartYear*12 + StartMonth)) %>%
## #   inner_join(title, by = c("TitleID" = "TitleId")) %>%
## #   filter(Field == "Founder") %>% 
## #   select(Name, Last.Name, First.Name, Year) %>%
## #   unite(Founder, First.Name, Last.Name, sep = " ") %>%
## #   distinct() %>%
## #   group_by(Name)
## # 
## # find_prior_founders <- function(df, full = prior_founder) {
## #   # Helper function to find prior founders of other startups
## #   #
## #   # Args:
## #   #  df: data.frame with founders for single startup
## #   #  full: data.frame for all startups/founders
## #   #
## #   # Returns:
## #   #  Fraction of founders who had experience as founders
## #   
## #   .startup <- df$Name[1]
## #   .year <- df$Year[1]
## #   
## #   prior <- full %>%
## #     filter(Founder %in% df$Founder, Name != .startup, Year < .year) %>%
## #     group_by(Founder) %>%
## #     summarise(n = n())
## #   
## #   data_frame(founder = nrow(prior) / nrow(df))
## # }
## # 
## # # !!! Long operation
## # prior_founder %<>% 
## #   do(find_prior_founders(.))
## # 
## # df.13 %<>% left_join(prior_founder, by = "Name")
## # df.13
## # 
## # # (a)
## # fit.a <- coxph(Surv(months, death) ~ experience, data = df.13)
## # summary(fit.a)
## # 
## # # (b)
## # fit.b <- coxph(Surv(months, death) ~ education, data = df.13)
## # summary(fit.b)
## # 
## # # (c)
## # fit.c <- coxph(Surv(months, death) ~ founder, data = df.13)
## # summary(fit.c)
## # 
## # # (a)-(c)
## # fit.ac <- coxph(Surv(months, death) ~ experience + education + founder, data = df.13)
## # summary(fit.ac)
## # 
## # # Q14--------------------------------------------------------------------------
## # # Merge the Github data that has y combinator funding data. 
## # # Create an outcome “received funding” and run an logit model with the same 
## # # set of covariates as in 13. Include a cohort fixed effect.  
## # 
## # df.14 <- tc_cohorts %>%
## #   mutate(Funding = str_replace_all(Funding, "[$,]", "") %>% as.numeric) %>%
## #   select(Name, funding = Funding) %>%
## #   left_join(df.13, by = "Name") %>%
## #   na.omit() %>%
## #   mutate(cohort = as.factor(cohort))
## # 
## # df.14
## # 
## # fit.1 <- felm(I(funding > 0) ~ log1p(experience) + log1p(education) + G(cohort), data = df.14)
## # 
## # fit.1 <- felm(log(funding) ~ log1p(experience) + log1p(education) + G(cohort), data = subset(df.14, funding > 0))
## # 
## # summary(fit.1)
## # 
## # 
## # m <- lm(death ~ log1p(experience)*log1p(education) + factor(cohort), data = df.14)
## # 
## # 
## # 
## # fit.2 <- lm(funding ~ experience + education + founder + factor(cohort), data = df.14)
## # summary(fit.2)
## # 
## # 
## # 
## # fit.3 <- glm(factor(death) ~ experience + education + founder + factor(cohort), 
## #              data = df.14, family = "binomial")
## # summary(fit.3)
## # 
## # 

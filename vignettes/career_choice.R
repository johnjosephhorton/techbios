library(devtools)

devtools::document("..")
devtools::install("~/Dropbox/tools/techbios")
library(techbios)

library(JJHmisc)
library(stargazer)


#######################
# YC - Tech Backgrounds
#######################

df.bg <- techbios::YCbackgrounds()

TechSummary <- function(data) {
       summarize(.data = data, 
                 CS.background = any(CS),
                 STEM.background = any(STEM),
                 TECH = any(CS) | any(STEM)
                 )
   }

# How many startup founders have a technical background? 
df.bg %>%
    group_by(First.Name, Last.Name) %>%
    TechSummary %>%
    summary %>% print

# How many startups have at least one founder w/ a a technical background? 
df.bg %>%
    group_by(Name) %>%
    TechSummary %>%
    summary %>%       
    print

#####################################################
#' Number of career transitions per person - Stanford 
#####################################################

df.choices <- techbios::StanfordCSPhDGradsCareerChoices() %>%
    filter(TimeDecisionMade > 1985 & TimeDecisionMade < 2015) %>% 
        mutate(
            periods = cut(TimeDecisionMade, seq(1960, 2015, 2)),
            delta = as.numeric(TimeDecisionMade) - as.numeric(ClassYear)
        )

# How many grads covered in this sample? 
df.choices %>% .$PersonId %>% unique %>% length

# How many stints in total? 
df.choices %>% nrow

# How many are of each type
df.choices %>% .$Engineer %>% sum
df.choices %>% .$Founder %>% sum


# how many engineers do both? 
df.choices %>%
    group_by(PersonId) %>% 
        summarize(TotalFounder = sum(Founder),
                  TotalEngineer = sum(Engineer),
                  AnyEngineer = I(sum(Engineer) > 0), 
                  AnyFounder = I(sum(Founder) > 0),
                  Both = AnyFounder & AnyEngineer
                  ) %>%
                      summary

# Distribution of jobs per person 
g <- df.choices %>%
      group_by(PersonId) %>%
      summarize(NumJobs = n()) %T>% (function(x) print(summary(x))) %>%
            ggplot(data = .) +
            geom_histogram(aes(x = NumJobs), binwidth = 1) +
            xlab("Number of jobs") +
            ylab("Number of graduates") +
            theme_bw()

print(g)

JJHmisc::writeImage(g, "num_jobs", path = ".")

#####################################################
#' Modeling Stanford graduates entrepreneurial choice
#####################################################

ModelToDF <- function(model, estimation) {
    if (class(model) == "lmerMod"){
        year = levels(model@frame$periods)
        effect = nlme::fixef(model)
        se = sqrt(diag(as.matrix(vcov(model))))
    } else {
        year = model$xlevels$periods %>% gsub("periods", "", .)
        effect = coef(model)
        se = sqrt(diag(vcov(model)))
    }  
    data.frame(year = year, effect = effect, se = se, estimation = estimation)
}

df.choices$su <- with(df.choices, Founder + Engineer)

m.ols <- lm(su ~ periods - 1, data = df.choices)
m.re <- lme4::lmer(su ~ periods - 1 + (1|PersonId), data = df.choices)


rbind(m.ols %>% ModelToDF(., "ols"), m.re %>% ModelToDF(., "re")) %T>%
print %>% 
    ggplot() +
        geom_point(aes(x = year, y = effect, colour = estimation),
                   position = "dodge") +
                       geom_errorbar(aes(x = year,
                                         ymin = effect - 2*se,
                                         ymax = effect + 2*se,
                                         colour = estimation)) + theme_bw() +
                    xlab("Year bins") +
                    ylab("Fraction of all job decisions that are entrepreneurial")


m.founder <- lme4::lmer(Founder ~ periods - 1 + (1|PersonId), data = df.choices)
m.engineer <- lme4::lmer(Engineer ~ periods - 1 + (1|PersonId), data = df.choices)


rbind(m.founder %>% ModelToDF(., "founder"),
      m.engineer %>% ModelToDF(., "engineer")) %>% 
    ggplot() +
        geom_point(aes(x = year,
                       y = effect,
                       colour = estimation), position = "dodge") +
            geom_errorbar(aes(x = year,
                              ymin = effect - 2*se,
                              ymax = effect + 2*se,
                              colour = estimation)) + theme_bw() +
                xlab("Year bins") +
                    ylab("Fraction of all job decisions that are entrepreneurial")


##############################################################
#' Entrepreneurship - how does prior experience affect things?  
##############################################################

library(lme4)
library(stargazer)

# Get the 
df.choices %>% group_by(PersonId) %>%
    mutate(
        Founder.prior = lag(Founder, order_by = TimeDecisionMade),
        Engineer.prior = lag(Engineer, order_by = TimeDecisionMade), 
        duration = lead(TimeDecisionMade,
            order_by = TimeDecisionMade) - TimeDecisionMade
    ) %>%
    mutate(
           duration.prior = lag(duration, order_by = TimeDecisionMade)
          ) %>% 
    list(
        "m.ef" = lmer(Engineer ~ Founder.prior + (1|PersonId), data = .),
        "m.ee" = lmer(Engineer ~ Engineer.prior + (1|PersonId), data = .),
        "m.fe" = lmer(Founder ~ Engineer.prior + (1|PersonId), data = .), 
        "m.ff" = lmer(Founder ~ Founder.prior + (1|PersonId), data = .)
    ) %>%
        (function(l) stargazer(l["m.ee"],l["m.fe"],
                               l["m.ef"], l["m.ff"], type = "text"))


# Probably need to consider whether same company has changed. 
df.choices %>% group_by(PersonId) %>%
    mutate(
        Founder.prior = lag(Founder, order_by = TimeDecisionMade),
        duration = lead(TimeDecisionMade,
            order_by = TimeDecisionMade) - TimeDecisionMade
    ) %>%
    mutate(
           duration.prior = lag(duration, order_by = TimeDecisionMade)
          ) %>% 
    list(
        "m.ols" = lm(Founder ~ Founder.prior + (1|PersonalId), data = .),
        "m.dur" = lm(Founder ~ Founder.prior + (1|PersonalId), data = .), 
        "m.re" = lmer(Founder ~ Founder.prior + (1|PersonId), data = .)
    ) %>%
        (function(l) stargazer(l["m.ols"], l["m.re"], l["m.dur"], type = "text"))




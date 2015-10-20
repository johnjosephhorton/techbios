## ----eval = FALSE--------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("johnjosephhorton/techbios",
#                           build_vignettes = TRUE)

## ---- message=FALSE------------------------------------------------------
library(techbios)

## ----fig.height=5, fig.width=7-------------------------------------------
# Shows data
data(df_1)
head(df_1)

# Plots data
StanfordCSPhDGradsPerYear()

## ----fig.height=5, fig.width=7-------------------------------------------
# Shows data
data(df_2)
head(df_2)

# Plots data
StanfordCSPhDGradsOccChoicePerYear()

## ----fig.height=5, fig.width=7-------------------------------------------
# Plots data
StanfordCSPhDGradsCareerHistories()

## ----fig.height=5, fig.width=7-------------------------------------------
# Plots data
StanfordCSPhDGradsTimeline(id = 2)

# The same, but only after graduation
StanfordCSPhDGradsTimeline(id = 2, grads_only = TRUE)

